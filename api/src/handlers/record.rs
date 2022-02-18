use crate::app_state::AppState;
use crate::db::{self, RecordType};
use crate::error::{AppError, ErrorType};
use actix_web::{web, HttpResponse};
use serde::{Deserialize, Serialize};
use tracing::info;

#[derive(Deserialize)]
pub struct RecordsQuery {
    /// The user search string provided by the user.
    #[serde(rename = "type")]
    type_: RecordType,
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
        None => {
            return Err(AppError {
                cause: format!("can't find puzzle slug {}", q.puzzle_slug),
                message: format!("can't find puzzle slug {}", q.puzzle_slug),
                error_type: ErrorType::NotFound,
            })
        }
    };
    info!("{}", puzzle_id);

    let records = db::fetch_records(&app_state.pool, q.type_, puzzle_id, q.page, 50).await?;

    Ok(HttpResponse::Ok().json(RecordsResponse { records }))
}

pub async fn record_api(
    record_id: web::Path<i32>,
    app_state: web::Data<AppState>,
) -> Result<HttpResponse, AppError> {
    let record_id = record_id.into_inner();
    let record = db::find_record(&app_state.pool, record_id).await?;

    match record {
        Some(record) => Ok(HttpResponse::Ok().json(record)),
        None => Err(AppError {
            cause: format!("can't find record {}", record_id),
            message: format!("can't find record {}", record_id),
            error_type: ErrorType::NotFound,
        }),
    }
}
