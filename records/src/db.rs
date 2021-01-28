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

#[derive(sqlx::FromRow, Debug)]
pub struct SingleResult {
    pub id: i32,
    pub time: i32,
    pub comment: Option<String>,
    pub penalty: Option<Penalty>,
    pub created_at: NaiveDateTime,
}

#[tracing::instrument(skip(pool))]
pub async fn fetch_singles(
    pool: &PgPool,
    user_id: u32,
    puzzle_id: u32,
) -> Result<Vec<SingleResult>, sqlx::Error> {
    sqlx::query_as(
        "SELECT id, comment, time, created_at, penalty FROM singles WHERE puzzle_id = $1 AND user_id = $2 ORDER BY created_at",
    )
    .bind(puzzle_id)
    .bind(user_id)
    .fetch_all(pool)
    .await
}
