use crate::app_state::AppState;
use crate::db;
use crate::error::{AppError, ErrorType};
use crate::extractors::LoggedInUser;
use actix_web::{web, HttpResponse};
use serde::Deserialize;

pub async fn post_api(
    post_id: web::Path<i32>,
    app_state: web::Data<AppState>,
) -> Result<HttpResponse, AppError> {
    let post_id = post_id.into_inner();
    let post = db::find_post(&app_state.pool, post_id).await?;

    match post {
        Some(post) => Ok(HttpResponse::Ok().json(post)),
        None => Err(AppError {
            cause: format!("can't find post {}", post_id),
            message: format!("can't find post {}", post_id),
            error_type: ErrorType::NotFound,
        }),
    }
}

#[derive(Deserialize)]
pub struct CreateComment {
    content: String,
}

pub async fn create_comment_api(
    post_id: web::Path<i32>,
    body: web::Json<CreateComment>,
    app_state: web::Data<AppState>,
    current_user: LoggedInUser,
) -> Result<HttpResponse, AppError> {
    let post = db::find_post(&app_state.pool, *post_id).await?;
    match post {
        Some(_post) => {
            let comment_id = db::create_comment(
                &app_state.pool,
                *post_id,
                current_user.user.id,
                body.content.clone(),
            )
            .await?;
            Ok(HttpResponse::Created().json(serde_json::json!({ "comment_id": comment_id })))
        }
        None => Err(AppError {
            cause: format!("post {} not found", *post_id),
            message: format!("post {} not found", *post_id),
            error_type: ErrorType::NotFound,
        }),
    }
}

pub async fn delete_comment_api(
    comment_id: web::Path<i32>,
    app_state: web::Data<AppState>,
    current_user: LoggedInUser,
) -> Result<HttpResponse, AppError> {
    let comment = db::fetch_comment(&app_state.pool, *comment_id).await?;

    match comment {
        Some(c) if current_user.user.is_admin() || current_user.user.id == c.user_id => {
            let deleted = db::delete_comment(&app_state.pool, *comment_id).await?;

            Ok(HttpResponse::Ok().json(serde_json::json!({ "deleted": deleted })))
        }
        _ => Err(AppError {
            cause: format!("comment {} not found", *comment_id),
            message: format!("comment {} not found", *comment_id),
            error_type: ErrorType::NotFound,
        }),
    }
}
