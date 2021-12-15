use actix_web::http;
use tracing::error;

#[derive(Debug)]
pub struct AppError {
    pub cause: String,
    pub message: String,
    pub error_type: ErrorType,
}

#[derive(Debug)]
pub enum ErrorType {
    JwtTokenInvalid,
    JwtSecretNotFound,
    NotFound,
    Unauthorized,
    DbError,
}

impl std::fmt::Display for AppError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match &self.error_type {
            ErrorType::JwtTokenInvalid => write!(f, "JWT token is invalid"),
            ErrorType::JwtSecretNotFound => write!(f, "JWT secret not found"),
            ErrorType::NotFound => write!(f, "Resource not found"),
            ErrorType::DbError => write!(f, "Db error"),
            ErrorType::Unauthorized => write!(f, "Not authorized"),
        }
    }
}

impl actix_web::error::ResponseError for AppError {
    fn status_code(&self) -> http::StatusCode {
        match self.error_type {
            ErrorType::NotFound => http::StatusCode::NOT_FOUND,
            ErrorType::JwtSecretNotFound => http::StatusCode::INTERNAL_SERVER_ERROR,
            ErrorType::JwtTokenInvalid => http::StatusCode::UNAUTHORIZED,
            ErrorType::DbError => http::StatusCode::INTERNAL_SERVER_ERROR,
            ErrorType::Unauthorized => http::StatusCode::UNAUTHORIZED,
        }
    }

    fn error_response(&self) -> actix_web::HttpResponse {
        error!("{:?}", self);
        actix_web::HttpResponse::NotFound().json(serde_json::json!({"error": "TBD"}))
    }
}

impl From<sqlx::Error> for AppError {
    fn from(e: sqlx::Error) -> Self {
        AppError {
            cause: format!("{:?}", e),
            message: "database access failed".to_string(),
            error_type: ErrorType::DbError,
        }
    }
}
