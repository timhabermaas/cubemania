use chrono::NaiveDateTime;
use serde::Serialize;
use sqlx::PgPool;

#[derive(sqlx::Type, Debug, Serialize, Clone)]
#[sqlx(rename = "varchar")]
pub enum Penalty {
    #[sqlx(rename = "plus2")]
    #[serde(rename = "PLUS2")]
    Plus2,
    #[sqlx(rename = "dnf")]
    #[serde(rename = "DNF")]
    Dnf,
}

#[derive(sqlx::FromRow, Debug, Clone)]
pub struct SingleResult {
    pub id: i32,
    pub time: i32,
    pub comment: Option<String>,
    pub penalty: Option<Penalty>,
    pub created_at: NaiveDateTime,
}

impl PartialEq for SingleResult {
    fn eq(&self, other: &Self) -> bool {
        if !self.is_solved() && !other.is_solved() {
            return true;
        }
        if self.is_solved() != other.is_solved() {
            return false;
        }
        self.time == other.time
    }
}

impl PartialOrd for SingleResult {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        if self == other {
            return Some(std::cmp::Ordering::Equal);
        }
        if self.is_solved() && !other.is_solved() {
            return Some(std::cmp::Ordering::Less);
        }
        if !self.is_solved() && other.is_solved() {
            return Some(std::cmp::Ordering::Greater);
        }
        self.time.partial_cmp(&other.time)
    }
}
impl Eq for SingleResult {}

impl Ord for SingleResult {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.partial_cmp(other).unwrap()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn single_ord_no_penalty() {
        let a = SingleResult {
            penalty: None,
            time: 12,
            comment: None,
            id: 12,
            created_at: NaiveDateTime::from_timestamp(100, 10),
        };
        let b = SingleResult {
            penalty: None,
            time: 13,
            comment: None,
            id: 12,
            created_at: NaiveDateTime::from_timestamp(100, 10),
        };
        assert!(a < b);
    }

    #[test]
    fn single_ord_one_penalty() {
        let a = SingleResult {
            penalty: Some(Penalty::Dnf),
            time: 12,
            comment: None,
            id: 12,
            created_at: NaiveDateTime::from_timestamp(100, 10),
        };
        let b = SingleResult {
            penalty: None,
            time: 13,
            comment: None,
            id: 12,
            created_at: NaiveDateTime::from_timestamp(100, 10),
        };
        assert!(a > b);

        let a = SingleResult {
            penalty: None,
            time: 12,
            comment: None,
            id: 12,
            created_at: NaiveDateTime::from_timestamp(100, 10),
        };
        let b = SingleResult {
            penalty: Some(Penalty::Dnf),
            time: 13,
            comment: None,
            id: 12,
            created_at: NaiveDateTime::from_timestamp(100, 10),
        };
        assert!(a < b);
    }
}

impl SingleResult {
    pub fn is_solved(&self) -> bool {
        !matches!(self.penalty, Some(Penalty::Dnf))
    }
}

#[tracing::instrument(skip(pool))]
pub async fn fetch_singles(
    pool: &PgPool,
    user_id: i32,
    puzzle_id: i32,
) -> Result<Vec<SingleResult>, sqlx::Error> {
    sqlx::query_as(
        "SELECT id, time, penalty, created_at, comment FROM singles WHERE user_id = $1 AND puzzle_id = $2 ORDER BY created_at",
    )
    .bind(user_id)
    .bind(puzzle_id)
    .fetch_all(pool)
    .await
}

#[derive(Debug)]
pub struct Record {
    pub set_at: NaiveDateTime,
    pub amount: u8,
    pub time: i32,
    pub comment: Option<String>,
    pub single_ids: Vec<i32>,
}

impl PartialEq for Record {
    fn eq(&self, other: &Self) -> bool {
        self.time == other.time
    }
}

impl Eq for Record {}

impl PartialOrd for Record {
    fn partial_cmp(&self, other: &Record) -> Option<std::cmp::Ordering> {
        self.time.partial_cmp(&other.time)
    }
}

impl Ord for Record {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.partial_cmp(other).unwrap()
    }
}

#[tracing::instrument(skip(pool, records))]
pub async fn set_records(
    pool: &sqlx::PgPool,
    records: &[Record],
    user_id: i32,
    puzzle_id: i32,
) -> Result<(), sqlx::Error> {
    let mut tx = pool.begin().await?;
    tracing::info!("finding all records");
    let ids: Vec<i32> =
        sqlx::query_as("SELECT id FROM records WHERE user_id = $1 AND puzzle_id = $2")
            .bind(user_id)
            .bind(puzzle_id)
            .fetch_all(&mut tx)
            .await?
            .into_iter()
            .map(|(x,)| x)
            .collect();
    tracing::info!("deleting all record associations");
    sqlx::query("DELETE FROM records_singles WHERE record_id = ANY($1)")
        .bind(ids)
        .execute(&mut tx)
        .await?;
    tracing::info!("deleting all records");
    sqlx::query("DELETE FROM records WHERE user_id = $1 AND puzzle_id = $2")
        .bind(user_id)
        .bind(puzzle_id)
        .execute(&mut tx)
        .await?;

    // TODO: Try optimizing by making SQL requests in parallel.
    for record in records {
        let (record_id,): (i32,) = sqlx::query_as("INSERT INTO records (time, puzzle_id, user_id, amount, set_at, created_at, updated_at) VALUES ($1, $2, $3, $4, $5, LOCALTIMESTAMP, LOCALTIMESTAMP) RETURNING id")
            .bind(record.time)
            .bind(puzzle_id)
            .bind(user_id)
            .bind(record.amount as i32)
            .bind(record.set_at)
            .fetch_one(&mut tx)
            .await?;
        for single_id in &record.single_ids {
            sqlx::query("INSERT INTO records_singles (single_id, record_id) VALUES ($1, $2)")
                .bind(single_id)
                .bind(record_id)
                .execute(&mut tx)
                .await?;
        }
    }
    tx.commit().await?;
    Ok(())
}

#[derive(sqlx::FromRow, Debug)]
pub struct DbJob {
    pub id: i32,
    pub handler: String,
}

#[tracing::instrument(skip(pool))]
pub async fn fetch_next_job(pool: &sqlx::PgPool) -> Result<Option<DbJob>, sqlx::Error> {
    sqlx::query_as("SELECT id, handler from delayed_jobs")
        .fetch_optional(pool)
        .await
}

#[tracing::instrument(skip(pool))]
pub async fn remove_job(pool: &sqlx::PgPool, id: i32) -> Result<(), sqlx::Error> {
    sqlx::query("DELETE FROM delayed_jobs WHERE id = $1")
        .bind(id)
        .execute(pool)
        .await
        .map(|_| ())
}
