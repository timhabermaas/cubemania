use crate::app_state::AppState;
use crate::db;
use crate::error::AppError;
use crate::extractors::{AdminUser, LoggedInUser};
use actix_web::{web, HttpResponse};
use serde::{Deserialize, Serialize};

#[derive(Serialize)]
struct MeResponse {
    current_user: Option<db::SimpleUser>,
}

pub async fn me_api(current_user: LoggedInUser) -> HttpResponse {
    HttpResponse::Ok().json(MeResponse {
        current_user: Some(current_user.user),
    })
}

#[derive(Deserialize)]
pub struct UsersQuery {
    /// The user search string provided by the user.
    q: Option<String>,
    page: Option<u32>,
}

#[derive(Serialize)]
struct UsersResponse {
    users: db::Paginated<db::SimpleUser, u32>,
}

pub async fn users_api(
    web::Query(q): web::Query<UsersQuery>,
    app_state: web::Data<AppState>,
) -> Result<HttpResponse, AppError> {
    let paginated = db::fetch_users(&app_state.pool, q.page, 200, q.q).await?;
    Ok(HttpResponse::Ok().json(UsersResponse { users: paginated }))
}

#[derive(Deserialize)]
pub struct UserBlockRequest {
    user_slug: String,
}

pub async fn user_block_api(
    path: web::Path<UserBlockRequest>,
    app_state: web::Data<AppState>,
    _current_admin: AdminUser,
) -> Result<HttpResponse, AppError> {
    let flag = db::block_user(&app_state.pool, &path.user_slug).await?;

    Ok(HttpResponse::Ok().json(serde_json::json!({ "ignored": flag })))
}
