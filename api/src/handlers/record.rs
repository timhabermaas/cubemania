use crate::app_state::AppState;
use crate::db;
use crate::error::AppError;
use actix_web::{web, HttpResponse};
use serde::{Deserialize, Serialize};
use tracing::info;

#[derive(Deserialize)]
pub struct RecordsQuery {
    /// The user search string provided by the user.
    #[serde(rename = "type")]
    type_: String,
    page: Option<u32>,
    puzzle_slug: String,
}

#[derive(Serialize)]
struct RecordsResponse {
    records: db::Paginated<db::RecordRow, u32>,
}

pub async fn records_api(
    web::Query(q): web::Query<RecordsQuery>,
    app_state: web::Data<AppState>,
) -> Result<HttpResponse, AppError> {
    info!("{}", q.puzzle_slug);
    let puzzle_id = db::fetch_puzzle_id_from_slug(&app_state.pool, &q.puzzle_slug).await?;

    let puzzle_id = match puzzle_id {
        Some(p) => p,
        None => return Ok(HttpResponse::NotFound().body("puzzle slug doesn't exist")),
    };
    info!("{}", puzzle_id);

    let records = db::fetch_records(&app_state.pool, &q.type_, puzzle_id, q.page, 50).await?;

    Ok(HttpResponse::Ok().json(RecordsResponse { records }))
}
