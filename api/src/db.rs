use chrono::NaiveDateTime;
use itertools::Itertools;
use serde::{Deserialize, Serialize};
use sqlx::PgPool;

#[derive(sqlx::Type, Debug, Serialize, Clone)]
#[sqlx(type_name = "varchar")]
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

#[derive(sqlx::Type, Debug, Serialize, Clone)]
#[sqlx(type_name = "varchar")]
pub enum UserRole {
    #[sqlx(rename = "admin")]
    #[serde(rename = "admin")]
    Admin,
    #[sqlx(rename = "moderator")]
    #[serde(rename = "moderator")]
    Moderator,
    #[sqlx(rename = "user")]
    #[serde(rename = "user")]
    User,
}

#[derive(sqlx::FromRow, Debug, Serialize, Clone)]
pub struct SimpleUser {
    pub id: i32,
    pub singles_count: i32,
    pub name: String,
    pub slug: String,
    role: UserRole,
}

impl SimpleUser {
    pub fn is_admin(&self) -> bool {
        matches!(self.role, UserRole::Admin)
    }
}

#[derive(Serialize)]
pub struct Paginated<T, I> {
    pub items: Vec<T>,
    pub page: I,
    pub max_items_per_page: usize,
    pub next_page: Option<I>,
    pub total_item_count: usize,
}

impl<T> Paginated<T, u32> {
    fn from_vec(
        mut vec: Vec<T>,
        page: u32,
        limit: u32,
        total_item_count: usize,
        max_items_per_page: usize,
    ) -> Paginated<T, u32> {
        let next_page = if vec.len() as u32 > limit {
            for _ in 0..(vec.len() as u32 - limit) {
                vec.pop();
            }
            Some(page + 1)
        } else {
            None
        };

        Paginated {
            max_items_per_page,
            items: vec,
            page,
            next_page,
            total_item_count,
        }
    }
}

#[tracing::instrument(skip(pool))]
pub async fn fetch_users(
    pool: &PgPool,
    // TODO: Switch to `after` based pagination to avoid scanning the entire table
    page: Option<u32>,
    limit: u32,
    search_term: Option<String>,
) -> Result<Paginated<SimpleUser, u32>, sqlx::Error> {
    let page = page.unwrap_or(1);

    let search_term_sql = search_term
        .map(|name| format!("%{}%", name.to_lowercase()))
        .unwrap_or("%".to_string());

    let users: Vec<SimpleUser> = sqlx::query_as(
        "SELECT id, name, slug, singles_count, role FROM users WHERE lower(name) LIKE $3 ORDER BY singles_count DESC LIMIT $1 OFFSET $2",
    )
    // Intentionally overfetching to determine whether there are more results to be returned.
    .bind(limit + 1)
    .bind((page - 1) * limit)
    .bind(search_term_sql.clone())
    .fetch_all(pool)
    .await?;

    let (count,): (i64,) = sqlx::query_as("SELECT count(*) FROM users WHERE lower(name) LIKE $1;")
        .bind(search_term_sql)
        .fetch_one(pool)
        .await?;

    Ok(Paginated::from_vec(
        users,
        page,
        limit,
        count as usize,
        limit as usize,
    ))
}

pub async fn fetch_max_singles_count(pool: &PgPool) -> Result<u64, sqlx::Error> {
    let foo: Option<(i32,)> = sqlx::query_as("SELECT MAX(singles_count) FROM users")
        .fetch_optional(pool)
        .await?;
    if let Some((count,)) = foo {
        Ok(count as u64)
    } else {
        Ok(0)
    }
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

pub async fn find_simple_user(
    pool: &sqlx::PgPool,
    user_id: i32,
) -> Result<Option<SimpleUser>, sqlx::Error> {
    sqlx::query_as("SELECT id, name, slug, singles_count, role from users WHERE id = $1")
        .bind(user_id)
        .fetch_optional(pool)
        .await
}

#[derive(Debug, Serialize, sqlx::FromRow)]
pub struct Post {
    id: i32,
    title: String,
    content: String,
    comments_count: i64,
}

pub async fn find_announcement(pool: &sqlx::PgPool) -> Result<Option<Post>, sqlx::Error> {
    let post: Option<(i32, String, String)> =
        sqlx::query_as("SELECT id, title, content FROM posts ORDER BY created_at DESC LIMIT 1")
            .fetch_optional(pool)
            .await?;

    if let Some(post) = post {
        let comment_result: Option<(i64,)> = sqlx::query_as(
            "SELECT COUNT(*) FROM comments WHERE commentable_id = $1 AND commentable_type = 'Post'",
        )
        .bind(post.0)
        .fetch_optional(pool)
        .await?;
        let comments_count = comment_result.map(|x| x.0).unwrap_or(0);
        Ok(Some(Post {
            id: post.0,
            content: post.2,
            title: post.1,
            comments_count,
        }))
    } else {
        Ok(None)
    }
}

#[derive(Debug, Serialize)]
pub struct Puzzle {
    id: i32,
    slug: String,
    css_position: i32,
    name: String,
}

#[derive(Debug, Serialize)]
pub struct Kind {
    id: i32,
    name: String,
    css_position: i32,
    puzzles: Vec<Puzzle>,
}

#[derive(sqlx::FromRow, Debug, Clone)]
struct PuzzleKindJoin {
    kind_id: i32,
    kind_name: String,
    kind_css_position: i32,
    puzzle_id: i32,
    puzzle_name: String,
    puzzle_slug: String,
    puzzle_css_position: i32,
}

pub async fn fetch_puzzles(pool: &sqlx::PgPool) -> Result<Vec<Kind>, sqlx::Error> {
    let q = "
      SELECT kinds.id as kind_id, kinds.name as kind_name, kinds.css_position as kind_css_position,
             puzzles.id as puzzle_id, puzzles.name as puzzle_name, puzzles.slug as puzzle_slug, puzzles.css_position as puzzle_css_position
      FROM kinds
      LEFT JOIN puzzles ON puzzles.kind_id=kinds.id
      ORDER BY kinds.id, puzzles.name";
    let r: Vec<PuzzleKindJoin> = sqlx::query_as(q).fetch_all(pool).await?;

    let mut result = Vec::new();
    for (kind, group) in &r
        .into_iter()
        .group_by(|e| (e.kind_id, e.kind_name.clone(), e.kind_css_position))
    {
        let puzzles = group
            .map(|pk| Puzzle {
                id: pk.puzzle_id,
                slug: pk.puzzle_slug,
                css_position: pk.puzzle_css_position,
                name: pk.puzzle_name,
            })
            .collect::<Vec<_>>();
        result.push(Kind {
            id: kind.0,
            name: kind.1,
            css_position: kind.2,
            puzzles,
        });
    }

    Ok(result)
}

pub async fn fetch_puzzle_id_from_slug(
    pool: &sqlx::PgPool,
    slug: &str,
) -> Result<Option<i32>, sqlx::Error> {
    let row: Option<(i32,)> = sqlx::query_as("SELECT id FROM puzzles WHERE slug = $1")
        .bind(slug)
        .fetch_optional(pool)
        .await?;

    Ok(row.map(|x| x.0))
}

pub async fn block_user(pool: &sqlx::PgPool, slug: &str) -> Result<bool, sqlx::Error> {
    let result = sqlx::query("UPDATE users SET ignored = true WHERE slug = $1")
        .bind(slug)
        .execute(pool)
        .await?;

    Ok(result.rows_affected() > 0)
}

#[derive(Debug, sqlx::FromRow, Serialize)]
pub struct RecordRow {
    id: i32,
    rank: i32,
    time: i32,
    user_name: String,
    user_slug: String,
    comment: Option<String>,
    set_at: NaiveDateTime,
}

// TODO: Use custom extractor instead.
#[derive(Deserialize, Clone, Copy)]
pub enum RecordType {
    #[serde(rename = "single")]
    Single,
    #[serde(rename = "avg5")]
    Avg5,
    #[serde(rename = "avg12")]
    Avg12,
}

pub async fn fetch_records(
    pool: &sqlx::PgPool,
    type_: RecordType,
    puzzle_id: i32,
    page: Option<u32>,
    limit: u32,
) -> Result<Paginated<RecordRow, u32>, sqlx::Error> {
    let page = page.unwrap_or(1);
    let amount = match type_ {
        RecordType::Single => 1,
        RecordType::Avg5 => 5,
        RecordType::Avg12 => 12,
    };
    let q = "
    SELECT records.id as id, 1 as rank, time, users.name as user_name, users.slug as user_slug, comment, set_at
    FROM records
    INNER JOIN users ON records.user_id = users.id
    WHERE puzzle_id = $1 AND amount = $2 AND users.ignored = false ORDER BY time
    LIMIT $3 OFFSET $4";
    let mut records: Vec<RecordRow> = sqlx::query_as(q)
        .bind(puzzle_id)
        .bind(amount)
        .bind(limit + 1)
        .bind((page - 1) * limit)
        .fetch_all(pool)
        .await?;

    for (i, mut r) in records.iter_mut().enumerate() {
        r.rank = 1 + (page as i32 - 1) * limit as i32 + i as i32;
    }

    let q = "
    SELECT count(*)
    FROM records
    INNER JOIN users ON records.user_id = users.id
    WHERE puzzle_id = $1 AND amount = $2 AND users.ignored = false";

    let (count,): (i64,) = sqlx::query_as(q)
        .bind(puzzle_id)
        .bind(amount)
        .fetch_one(pool)
        .await?;

    Ok(Paginated::from_vec(
        records,
        page,
        limit,
        count as usize,
        limit as usize,
    ))
}

#[derive(sqlx::FromRow, Debug, Clone)]
pub struct RecordWithoutSingles {
    pub id: i32,
    pub set_at: NaiveDateTime,
    pub amount: i32,
    pub time: i32,
    pub user_name: String,
    pub user_slug: String,
    pub puzzle_css_position: i32,
    pub kind_css_position: i32,
    pub puzzle_name: String,
    pub kind_short_name: Option<String>,
    pub comment: Option<String>,
}

#[derive(sqlx::FromRow, Debug, Clone, Serialize)]
pub struct RecordWithSingles {
    pub id: i32,
    pub set_at: NaiveDateTime,
    pub amount: i32,
    pub time: i32,
    pub user_name: String,
    pub user_slug: String,
    pub puzzle_css_position: i32,
    pub kind_css_position: i32,
    pub puzzle_name: String,
    pub kind_short_name: Option<String>,
    pub comment: Option<String>,
    pub singles: Vec<SingleResultWithScramble>,
}

#[derive(sqlx::FromRow, Debug, Clone, Serialize)]
pub struct SingleResultWithScramble {
    pub id: i32,
    pub time: i32,
    pub comment: Option<String>,
    pub penalty: Option<Penalty>,
    pub created_at: NaiveDateTime,
    pub scramble: String,
}

pub async fn fetch_record(
    pool: &PgPool,
    record_id: i32,
) -> Result<Option<RecordWithSingles>, sqlx::Error> {
    let record_without_singles: Option<RecordWithoutSingles> =
        sqlx::query_as(
            "SELECT p.css_position as puzzle_css_position, k.css_position as kind_css_position, p.name as puzzle_name,
                    k.short_name as kind_short_name, records.id, set_at, amount, time, comment, u.name as user_name, u.slug as user_slug
            FROM records
            INNER JOIN puzzles p ON p.id = records.puzzle_id
            INNER JOIN kinds k ON p.kind_id = k.id
            INNER JOIN users u ON u.id = records.user_id
            WHERE records.id = $1"
            )
            .bind(record_id)
            .fetch_optional(pool)
            .await?;

    if record_without_singles.is_none() {
        return Ok(None);
    }
    let record_without_singles = record_without_singles.unwrap();

    let singles: Vec<SingleResultWithScramble> = sqlx::query_as(
        "SELECT id, time, comment, penalty, created_at, scramble
        FROM singles s, records_singles rs
        WHERE rs.single_id = s.id AND rs.record_id = $1
        ORDER BY s.created_at;
        ",
    )
    .bind(record_id)
    .fetch_all(pool)
    .await?;

    Ok(Some(RecordWithSingles {
        id: record_without_singles.id,
        set_at: record_without_singles.set_at,
        amount: record_without_singles.amount,
        kind_css_position: record_without_singles.kind_css_position,
        puzzle_css_position: record_without_singles.puzzle_css_position,
        puzzle_name: record_without_singles.puzzle_name,
        kind_short_name: record_without_singles.kind_short_name,
        time: record_without_singles.time,
        comment: record_without_singles.comment,
        singles,
        user_name: record_without_singles.user_name,
        user_slug: record_without_singles.user_slug,
    }))
}
