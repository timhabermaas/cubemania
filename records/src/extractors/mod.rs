use crate::app_state::AppState;
use crate::db;
use crate::error::{AppError, ErrorType};
use actix_web::{dev, web, FromRequest, HttpRequest};
use futures::Future;
use futures_util::future::{ok, Ready};
use jwt::{decode, Algorithm, DecodingKey, Validation};
use serde::Deserialize;
use std::collections::HashMap;
use std::pin::Pin;
use tracing::info;

#[derive(Deserialize, Debug)]
struct JwtClaim {
    user_id: i32,
}

#[derive(Debug, Clone)]
pub struct UserSession {
    pub user_id: Option<i32>,
}

impl FromRequest for UserSession {
    type Error = AppError;
    type Future = Ready<Result<Self, Self::Error>>;

    fn from_request(req: &HttpRequest, _payload: &mut dev::Payload) -> Self::Future {
        let app_state = req
            .app_data::<web::Data<AppState>>()
            .expect("app data not found");

        let token = req.headers().get("Authorization").map(|content| {
            content
                .to_str()
                .expect("must be string")
                .trim_start_matches("Bearer ")
                .to_string()
        });
        let query = parse_query_string(req.query_string()).remove("token");
        // First try looking for authorization token, fallback to URL param otherwise.
        if let Some(token) = token.or(query) {
            let token_data = decode::<JwtClaim>(
                &token,
                &DecodingKey::from_secret(app_state.jwt_secret.as_ref()),
                &Validation::new(Algorithm::HS256),
            );

            if let Err(e) = token_data {
                info!("jwt token not valid, {:?}", e);
                ok(UserSession { user_id: None })
            } else {
                ok(UserSession {
                    user_id: Some(token_data.unwrap().claims.user_id),
                })
            }
        } else {
            info!("jwt token not found");
            ok(UserSession { user_id: None })
        }
    }
}

fn parse_query_string(query_string: &str) -> HashMap<String, String> {
    let parts = query_string.split("&");
    let mut result = HashMap::new();
    for part in parts {
        let mut key_value = part.splitn(2, "=");
        if let Some(key) = key_value.next() {
            if let Some(value) = key_value.next() {
                result.insert(key.to_string(), value.to_string());
            }
        }
    }
    result
}

#[derive(Debug, Clone)]
pub struct LoggedInUser {
    pub user: db::SimpleUser,
}

impl FromRequest for LoggedInUser {
    type Error = AppError;
    type Future = Pin<Box<dyn Future<Output = Result<Self, Self::Error>>>>;

    fn from_request(req: &HttpRequest, payload: &mut dev::Payload) -> Self::Future {
        let session = UserSession::from_request(req, payload).into_inner();
        let app_state = req
            .app_data::<web::Data<AppState>>()
            .expect("app data not found")
            .clone();

        Box::pin(async move {
            if let Ok(session) = session {
                let user = current_user(&app_state, session.clone()).await?;
                if let Some(user) = user {
                    Ok(Self { user })
                } else {
                    Err(AppError {
                        cause: "expected existing user for session".to_string(),
                        message: "expected existing user for session".to_string(),
                        error_type: ErrorType::Unauthorized,
                    })
                }
            } else {
                Err(AppError {
                    cause: "expected existing session".to_string(),
                    message: "expected existing session".to_string(),
                    error_type: ErrorType::Unauthorized,
                })
            }
        })
    }
}

async fn current_user(
    app_state: &web::Data<AppState>,
    user_session: UserSession,
) -> Result<Option<db::SimpleUser>, AppError> {
    let current_user = if let Some(user_id) = user_session.user_id {
        db::find_simple_user(&app_state.pool, user_id).await?
    } else {
        None
    };

    Ok(current_user)
}

#[derive(Debug, Clone)]
pub struct AdminUser {
    pub user: db::SimpleUser,
}

impl FromRequest for AdminUser {
    type Error = AppError;
    type Future = Pin<Box<dyn Future<Output = Result<Self, Self::Error>>>>;

    fn from_request(req: &HttpRequest, payload: &mut dev::Payload) -> Self::Future {
        let req = req.clone();

        let user_future = LoggedInUser::from_request(&req, payload);

        Box::pin(async move {
            let current_user = user_future.await?;
            if current_user.user.is_admin() {
                Ok(AdminUser {
                    user: current_user.user,
                })
            } else {
                return Err(AppError {
                    cause: "user is not an admin".to_string(),
                    message: "user is not an admin".to_string(),
                    error_type: ErrorType::Unauthorized,
                });
            }
        })
    }
}
