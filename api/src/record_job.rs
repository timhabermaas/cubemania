use serde::Deserialize;
use sqlx::postgres::PgListener;

// TODO: Explore something like anyhow/thiserror to make the entire thing more ergonomic. E.g. it's
// tedious to manually add context to error messages.
#[derive(Debug)]
enum JobError {
    ParsingError { context: String, message: String },
    DbError(sqlx::Error),
}

impl From<sqlx::Error> for JobError {
    fn from(e: sqlx::Error) -> Self {
        JobError::DbError(e)
    }
}

#[tracing::instrument(skip(pool))]
async fn handle_job(pool: &sqlx::PgPool, user_id: i32, puzzle_id: i32) -> Result<(), JobError> {
    // TODO: There's probably a race condition hiding here if the rails server
    // saves a new single which triggers a new record. Locking table?
    // TODO: Try optimizing by using streaming. Currently the entire singles dataset is loaded into
    // memory at once.
    let singles = crate::db::fetch_singles(pool, user_id, puzzle_id).await?;
    let records = calculate_records(&singles);
    crate::db::set_records(pool, &records, user_id, puzzle_id).await?;
    Ok(())
}

#[derive(Deserialize, Debug)]
struct RecordCalculationMessage {
    user_id: i32,
    puzzle_id: i32,
}

pub async fn runner(pool: sqlx::PgPool) {
    let mut listener = PgListener::connect_with(&pool)
        .await
        .expect("failed to connect to listener");
    listener
        .listen("record_chan")
        .await
        .expect("registering channel to listen on failed");

    loop {
        let notification = listener.recv().await.expect("listening failed");
        let message: RecordCalculationMessage =
            serde_json::from_str(notification.payload()).expect("failed to decode chan payload");
        handle_job(&pool, message.user_id, message.puzzle_id)
            .await
            .expect("recalculating record failed");
    }
}

#[tracing::instrument(skip(singles), fields(singles_count = singles.len()))]
fn calculate_records(singles: &[crate::db::SingleResult]) -> Vec<crate::db::Record> {
    let mut records = vec![];

    if let Some(single_record) =
        singles
            .iter()
            .filter(|s| s.is_solved())
            .min()
            .map(|s| crate::db::Record {
                set_at: s.created_at,
                amount: 1,
                time: s.time,
                comment: s.comment.clone(),
                single_ids: vec![s.id],
            })
    {
        records.push(single_record);
    }

    if let Some(AverageResult::Success(record)) =
        singles.windows(5).map(|w| cubing_average(w)).min()
    {
        records.push(record);
    }
    if let Some(AverageResult::Success(record)) =
        singles.windows(12).map(|w| cubing_average(w)).min()
    {
        records.push(record);
    }
    records
}

// NOTE: Deriving ords for enums relies on the order of the variants as defined in source code, so
// don't change it!
#[derive(Debug, PartialOrd, PartialEq, Eq, Ord)]
enum AverageResult {
    Success(crate::db::Record),
    Dnf,
}

fn cubing_average(singles: &[crate::db::SingleResult]) -> AverageResult {
    // Ensuring we have at least 3 singles makes the `unwrap` below safe.
    if singles.len() < 3 {
        return AverageResult::Dnf;
    }

    let mut s = singles.to_vec();
    s.sort();

    // SAFETY: `unwrap` and slicing is safe since we guard against singles having less than 3
    // entries.
    let middle = &s[1..s.len() - 1];
    if !middle.last().unwrap().is_solved() {
        return AverageResult::Dnf;
    }

    let time: i64 = middle
        .iter()
        .filter(|s| s.is_solved())
        .map(|s| s.time as i64)
        .sum();

    AverageResult::Success(crate::db::Record {
        time: ((time as f64) / (middle.len() as f64)).floor() as i32,
        single_ids: singles.iter().map(|s| s.id).collect(),
        comment: singles
            .iter()
            .filter(|s| s.comment.is_some())
            .map(|s| s.comment.clone())
            .collect::<Option<Vec<_>>>()
            .map(|comments| comments.join("; ")),
        amount: singles.len() as u8,
        set_at: singles.last().unwrap().created_at,
    })
}

#[cfg(test)]
mod cubing_average_tests {
    use crate::db::Penalty::*;
    use chrono::NaiveDateTime;

    use super::*;

    fn build_singles(times: &[(i32, Option<crate::db::Penalty>)]) -> Vec<crate::db::SingleResult> {
        times
            .iter()
            .enumerate()
            .map(|(i, (time, penalty))| crate::db::SingleResult {
                created_at: NaiveDateTime::from_timestamp(100 + i as i64, 23),
                comment: if penalty.is_none() {
                    Some(format!("{}", i))
                } else {
                    None
                },
                id: i as i32,
                time: *time,
                penalty: penalty.clone(),
            })
            .collect()
    }

    #[test]
    fn too_few_singles() {
        assert_eq!(cubing_average(&vec![]), AverageResult::Dnf);
        assert_eq!(
            cubing_average(&build_singles(&vec![(2, None)])),
            AverageResult::Dnf
        );
        assert_eq!(
            cubing_average(&build_singles(&vec![(1, None), (2, None)])),
            AverageResult::Dnf
        );
    }

    #[test]
    fn all_dnf() {
        let singles = build_singles(&vec![
            (1, Some(Dnf)),
            (1, Some(Dnf)),
            (1, Some(Dnf)),
            (1, Some(Dnf)),
        ]);
        assert_eq!(cubing_average(&singles), AverageResult::Dnf);
    }

    #[test]
    fn some_valid_avg5() {
        let singles = build_singles(&vec![
            (10, None),
            (100, None),
            (32, None),
            (10, Some(Dnf)),
            (42, None),
        ]);
        if let AverageResult::Success(record) = cubing_average(&singles) {
            assert_eq!(record.time, 58);
            assert_eq!(record.amount, 5);
            assert_eq!(record.set_at, singles.last().unwrap().created_at);
            assert_eq!(
                record.single_ids,
                singles.iter().map(|x| x.id).collect::<Vec<_>>()
            );
            assert_eq!(record.comment, Some("0; 1; 2; 4".to_string()));
        } else {
            panic!("didn't return a record");
        }
    }

    #[test]
    fn some_avg5_with_too_many_dnfs() {
        let singles = build_singles(&vec![
            (1, Some(Dnf)),
            (100, None),
            (32, None),
            (2, Some(Dnf)),
            (42, None),
        ]);
        assert_eq!(cubing_average(&singles), AverageResult::Dnf);
    }
}
