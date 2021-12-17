use crate::app_state::AppState;
use crate::db;
use crate::error::AppError;
use crate::extractors::LoggedInUser;
use actix_web::{web, HttpResponse, Responder};
use chrono::NaiveDateTime;
use serde::{Deserialize, Serialize};
use tracing::{debug, info};

#[derive(Serialize)]
struct MaxSinglesCountResponse {
    max_singles_record: u64,
}

pub async fn max_singles_count_api(
    app_state: web::Data<AppState>,
) -> Result<HttpResponse, AppError> {
    let max_singles_record = db::fetch_max_singles_count(&app_state.pool).await?;

    Ok(HttpResponse::Ok().json(MaxSinglesCountResponse { max_singles_record }))
}

#[derive(Deserialize)]
pub struct SinglesQuery {
    user_id: u32,
    puzzle_id: u32,
}

#[derive(Serialize)]
struct CsvRow<'a> {
    #[serde(rename = "time (including +2 penalty)")]
    time: i32,
    penalty: Option<db::Penalty>,
    comment: &'a str,
    #[serde(rename = "timestamp")]
    created_at: NaiveDateTime,
}

#[tracing::instrument(skip(singles))]
fn create_csv_from_singles(singles: &[db::SingleResult]) -> Result<String, csv::Error> {
    let mut buffer = vec![];
    {
        let mut wtr = csv::WriterBuilder::new()
            .delimiter(b';')
            .from_writer(&mut buffer);

        for s in singles {
            wtr.serialize(CsvRow {
                // TODO: Avoid clone and allocation
                comment: &s.comment.clone().unwrap_or("".to_string()),
                time: s.time,
                created_at: s.created_at,
                penalty: s.penalty.clone(),
            })?;
        }

        wtr.flush()?;
    }
    info!("CSV file successfully created, bytes={}", buffer.len());
    Ok(String::from_utf8_lossy(&buffer).into_owned())
}

// Responder can be used for Result<T, E> as well as long as T: impl Responder and E: Into<Error>.
#[tracing::instrument(
    skip(q, app_state),
    fields(puzzle_id = q.puzzle_id, user_id = q.user_id)
)]
pub async fn singles_csv(
    web::Query(q): web::Query<SinglesQuery>,
    app_state: web::Data<AppState>,
    current_user: LoggedInUser,
) -> Result<impl Responder, AppError> {
    if current_user.user.id != q.user_id as i32 {
        // TODO: error
        // 404 is correct, but also needs to be logged and get proper error response.
        info!(
            "claims to be {:?}, but wants files for {:?}",
            current_user.user.id, q.user_id
        );
        return Ok(HttpResponse::NotFound().body("csv file not found"));
    }

    // TODO: proper error handling
    let singles = db::fetch_singles(&app_state.pool, q.user_id as i32, q.puzzle_id as i32).await?;

    debug!("{} singles fetched", singles.len());

    // TODO: Handle failures by "logging" to error!
    let csv = create_csv_from_singles(&singles).expect("should never fail");

    Ok(HttpResponse::Ok()
        .append_header((
            "Content-Disposition",
            "attachment; filename=\"singles.csv\"",
        ))
        .append_header(("Content-Type", "text/csv"))
        .body(csv))
}
