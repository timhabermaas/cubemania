use crate::app_state::AppState;
use crate::db;
use crate::error::AppError;
use actix_web::{web, HttpResponse};

pub async fn puzzles_api(app_state: web::Data<AppState>) -> Result<HttpResponse, AppError> {
    let kinds = db::fetch_puzzles(&app_state.pool).await?;

    Ok(HttpResponse::Ok().json(kinds))
}
