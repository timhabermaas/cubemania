use std::time::Duration;

use regex::Regex;

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
async fn poll_job(pool: &sqlx::PgPool) -> Result<bool, JobError> {
    match crate::db::fetch_next_job(pool).await? {
        Some(job) => {
            tracing::info!("job found: {:?}", job);

            // TODO: Wrap in transaction
            let (user_id, puzzle_id) = extract_user_id_and_puzzle_id_from_job(&job)?;

            handle_job(pool, user_id, puzzle_id).await?;

            crate::db::remove_job(pool, job.id).await?;

            // Returning true to indicate successful job handling => caller doesn't need to delay.
            return Ok(true);
        }
        None => Ok(false),
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

pub async fn runner(pool: sqlx::PgPool) {
    loop {
        match poll_job(&pool).await {
            // If we found a job we don't wait, but keep polling.
            Ok(true) => continue,
            Ok(false) => {}
            Err(e) => tracing::error!("Error while polling job: {:?}", e),
        }
        actix_rt::time::delay_for(Duration::from_secs(10)).await;
    }
}

fn extract_user_id_and_puzzle_id_from_job(job: &crate::db::DbJob) -> Result<(i32, i32), JobError> {
    lazy_static! {
        static ref RE: Regex =
            Regex::new(r"--- !ruby/struct:RecordCalculationJob\nuser_id: (\d+)\npuzzle_id: (\d+)")
                .unwrap();
    }
    let handler = &job.handler;
    let capture = RE.captures(&handler).ok_or(JobError::ParsingError {
        context: handler.clone(),
        message: format!("Couldn't match regex with {}", handler),
    })?;
    let user_id: i32 = capture
        .get(1)
        .ok_or(JobError::ParsingError {
            context: handler.clone(),
            message: "user_id not found".to_string(),
        })?
        .as_str()
        .parse()
        .map_err(|_| JobError::ParsingError {
            context: handler.clone(),
            message: "user_id must be a string".to_string(),
        })?;
    let puzzle_id: i32 = capture
        .get(2)
        .ok_or(JobError::ParsingError {
            context: handler.clone(),
            message: "puzzle_id not found".to_string(),
        })?
        .as_str()
        .parse()
        .map_err(|_| JobError::ParsingError {
            context: handler.clone(),
            message: "puzzle_id must be a string".to_string(),
        })?;
    Ok((user_id, puzzle_id))
}

#[tracing::instrument(skip(singles), fields(singles_count = singles.len()))]
fn calculate_records(singles: &[crate::db::SingleResult]) -> Vec<crate::db::Record> {
    let mut records = vec![];

    if let Some(single_record) =
        singles
            .iter()
            .filter(|s| s.is_valid())
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
    // Ensuring we have at least 3 singles makes the `unwrap` later safe.
    if singles.len() < 3 {
        return AverageResult::Dnf;
    }

    let mut s = singles.to_vec();
    s.sort();

    // SAFETY: `unwrap` and slicing is safe since we guard against singles having less than 3
    // entries.
    let middle = &s[1..s.len() - 1];
    if !middle.last().unwrap().is_valid() {
        return AverageResult::Dnf;
    }

    let time: i64 = middle
        .iter()
        .filter(|s| s.is_valid())
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
    use chrono::NaiveDateTime;

    use super::*;

    fn build_singles(times: &[Option<i32>]) -> Vec<crate::db::SingleResult> {
        times
            .iter()
            .enumerate()
            .map(|(i, t)| crate::db::SingleResult {
                created_at: NaiveDateTime::from_timestamp(100 + i as i64, 23),
                comment: if t.is_some() {
                    Some(format!("{}", i))
                } else {
                    None
                },
                id: i as i32,
                time: t.unwrap_or(0),
                penalty: if t.is_some() {
                    None
                } else {
                    Some(crate::db::Penalty::Dnf)
                },
            })
            .collect()
    }

    #[test]
    fn too_few_singles() {
        assert_eq!(cubing_average(&vec![]), AverageResult::Dnf);
        assert_eq!(
            cubing_average(&build_singles(&vec![Some(2)])),
            AverageResult::Dnf
        );
        assert_eq!(
            cubing_average(&build_singles(&vec![Some(1), Some(2)])),
            AverageResult::Dnf
        );
    }

    #[test]
    fn all_dnf() {
        let singles = build_singles(&vec![None, None, None, None]);
        assert_eq!(cubing_average(&singles), AverageResult::Dnf);
    }

    #[test]
    fn some_valid_avg5() {
        let singles = build_singles(&vec![Some(10), Some(100), Some(32), None, Some(42)]);
        // TODO: This test is pretty useless since it uses the custom eq instance which means we're
        // only comparing .time.
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
        let singles = build_singles(&vec![None, Some(100), Some(32), None, Some(42)]);
        // TODO: This test is pretty useless since it uses the custom eq instance which means we're
        // only comparing .time.
        assert_eq!(cubing_average(&singles), AverageResult::Dnf);
    }
}
