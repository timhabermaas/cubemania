extern crate jsonwebtoken as jwt;

mod db;
mod marshal;
mod record_job;

use actix_web::{
    dev, error::ResponseError, http::StatusCode, web, App, FromRequest, HttpRequest, HttpResponse,
    HttpServer, Responder,
};
use chrono::{DateTime, NaiveDateTime, Utc};
use futures_util::future::{err, ok, Ready};
use jwt::{decode, Algorithm, DecodingKey, Validation};
use record_job::runner;
use serde::{Deserialize, Serialize};
use sqlx::postgres::PgPoolOptions;
use sqlx::PgPool;
use std::collections::HashMap;
use std::env;
use tracing::subscriber::set_global_default;
use tracing::{debug, error, info};
use tracing_actix_web::TracingLogger;
use tracing_log::LogTracer;

#[derive(Deserialize, Debug)]
struct JwtClaim {
    user_id: i32,
}

#[derive(Debug)]
struct Single {
    time: i32,
    dnf: bool,
    created_at: DateTime<Utc>,
    comment: String,
}

#[derive(Deserialize)]
struct SinglesQuery {
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

#[derive(Debug)]
struct AppError {
    cause: String,
    message: String,
    error_type: ErrorType,
}

#[derive(Debug)]
enum ErrorType {
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

impl ResponseError for AppError {
    fn status_code(&self) -> StatusCode {
        match self.error_type {
            ErrorType::NotFound => StatusCode::NOT_FOUND,
            ErrorType::JwtSecretNotFound => StatusCode::INTERNAL_SERVER_ERROR,
            ErrorType::JwtTokenInvalid => StatusCode::UNAUTHORIZED,
            ErrorType::DbError => StatusCode::INTERNAL_SERVER_ERROR,
            ErrorType::Unauthorized => StatusCode::UNAUTHORIZED,
        }
    }

    fn error_response(&self) -> HttpResponse {
        error!("{:?}", self);
        HttpResponse::NotFound().json(
            [("error", "TBD")]
                .iter()
                .cloned()
                .collect::<HashMap<_, _>>(),
        )
    }
}

#[derive(Deserialize)]
struct UsersQuery {
    /// The user search string provided by the user.
    q: Option<String>,
    page: Option<u32>,
}

#[derive(Serialize)]
struct UsersResponse {
    users: db::Paginated<db::SimpleUser, u32>,
}

async fn users_api(
    web::Query(q): web::Query<UsersQuery>,
    app_state: web::Data<AppState>,
) -> Result<HttpResponse, AppError> {
    let paginated = db::fetch_users(&app_state.pool, q.page, 200, q.q)
        .await
        .map_err(|e| AppError {
            cause: format!("{:?}", e),
            message: "fetching singles failed".to_string(),
            error_type: ErrorType::DbError,
        })?;
    Ok(HttpResponse::Ok().json(UsersResponse { users: paginated }))
}

async fn announcement_api(app_state: web::Data<AppState>) -> Result<HttpResponse, AppError> {
    let post = db::find_announcement(&app_state.pool)
        .await
        .map_err(|e| AppError {
            cause: format!("{:?}", e),
            message: "fetching singles failed".to_string(),
            error_type: ErrorType::DbError,
        })?;
    Ok(HttpResponse::Ok().json(post))
}

#[derive(Serialize)]
struct MaxSinglesCountResponse {
    max_singles_record: u64,
}

async fn max_singles_record_api(app_state: web::Data<AppState>) -> Result<HttpResponse, AppError> {
    let max_singles_record = db::fetch_max_singles_count(&app_state.pool)
        .await
        .map_err(|e| AppError {
            cause: format!("{:?}", e),
            message: "fetching singles failed".to_string(),
            error_type: ErrorType::DbError,
        })?;

    Ok(HttpResponse::Ok().json(MaxSinglesCountResponse { max_singles_record }))
}

#[derive(Serialize)]
struct MeResponse {
    current_user: Option<db::SimpleUser>,
}
async fn me_api(
    app_state: web::Data<AppState>,
    user_session: UserSession,
) -> Result<HttpResponse, AppError> {
    let current_user = if let Some(user_id) = user_session.user_id {
        db::find_simple_user(&app_state.pool, user_id)
            .await
            .map_err(|e| AppError {
                cause: format!("{:?}", e),
                message: "fetching single user failed".to_string(),
                error_type: ErrorType::DbError,
            })?
    } else {
        None
    };

    Ok(HttpResponse::Ok().json(MeResponse { current_user }))
}

#[derive(Debug)]
struct UserSession {
    user_id: Option<i32>,
}

#[derive(Debug)]
struct LoggedInUser {
    user_id: i32,
}

struct UserSessionConfig {
    jwt_secret: String,
}

/// Debug trait implemented by hand to not leak secrets accidentally.
impl std::fmt::Debug for UserSessionConfig {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        fmt.debug_struct("UserSessionConfig")
            .field("jwt_secret", &"****")
            .finish()
    }
}

impl Default for UserSessionConfig {
    fn default() -> Self {
        // TODO: Is a default safe to use? Maybe better use a global attached to the binary on
        // startup?
        Self {
            jwt_secret: "nope".to_string(),
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

impl FromRequest for LoggedInUser {
    type Error = AppError;
    type Future = Ready<Result<Self, Self::Error>>;

    fn from_request(req: &HttpRequest, payload: &mut dev::Payload) -> Self::Future {
        let session = UserSession::from_request(req, payload).into_inner();
        if let Ok(session) = session {
            if let Some(user_id) = session.user_id {
                ok(Self { user_id })
            } else {
                err(AppError {
                    cause: "expected existing user for session".to_string(),
                    message: "expected existing user for session".to_string(),
                    error_type: ErrorType::Unauthorized,
                })
            }
        } else {
            err(AppError {
                cause: "expected existing session".to_string(),
                message: "expected existing session".to_string(),
                error_type: ErrorType::Unauthorized,
            })
        }
    }
}

impl FromRequest for UserSession {
    type Error = AppError;
    type Future = Ready<Result<Self, Self::Error>>;

    fn from_request(req: &HttpRequest, _payload: &mut dev::Payload) -> Self::Future {
        let app_state = req.app_data::<web::Data<AppState>>();
        if app_state.is_none() {
            return err(AppError {
                cause: "jwt secret not found".to_string(),
                message: "jwt secret not found".to_string(),
                error_type: ErrorType::JwtSecretNotFound,
            });
        }

        let app_state = app_state.unwrap();

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

// Responder can be used for Result<T, E> as well as long as T: impl Responder and E: Into<Error>.
#[tracing::instrument(
    skip(q, app_state),
    fields(puzzle_id = q.puzzle_id, user_id = q.user_id)
)]
async fn singles_csv(
    web::Query(q): web::Query<SinglesQuery>,
    app_state: web::Data<AppState>,
    current_user: LoggedInUser,
) -> Result<impl Responder, AppError> {
    if current_user.user_id != q.user_id as i32 {
        // TODO: error
        // 404 is correct, but also needs to be logged and get proper error response.
        info!(
            "claims to be {:?}, but wants files for {:?}",
            current_user.user_id, q.user_id
        );
        return Ok(HttpResponse::NotFound().body("csv file not found"));
    }

    // TODO: proper error handling
    let singles = db::fetch_singles(&app_state.pool, q.user_id as i32, q.puzzle_id as i32)
        .await
        .map_err(|e| AppError {
            cause: format!("{:?}", e),
            message: "fetching singles failed".to_string(),
            error_type: ErrorType::DbError,
        })?;

    debug!("{} singles fetched", singles.len());

    // TODO: Handle failures by "logging" to error!
    let csv = create_csv_from_singles(&singles).expect("should never fail");

    Ok(HttpResponse::Ok()
        .append_header((
            "Content-Disposition",
            "attachment; filename=\"singles.csv\"",
        ))
        .append_header(("Content-Type", "text/csv"))
        //.append_header(("Content-Type", "text/plain"))
        .body(csv))
}

#[derive(Clone)]
struct AppState {
    pool: PgPool,
    jwt_secret: String,
}

#[derive(Debug)]
struct SessionData {
    user_id: i64,
    csrf_token: String,
}

fn parse_session(data: &str) -> Option<SessionData> {
    let binary = base64::decode(data).ok()?;
    let decoded = marshal::decode(&binary).expect("couldn't decode");
    match decoded {
        marshal::RubyObject::Hash(m) => {
            println!("{:?}", m);
        }
        _ => {
            return None;
        }
    }
    None
}

#[actix_web::main]
async fn main() -> std::io::Result<()> {
    /*
    let session_data = "BAh7B0kiBmEGOgZFVEkiBmMGOwBUSSIGeAY7AFRJIgZ5BjsAVA==";
    let result = dbg!(parse_session(session_data));
    let binary = base64::decode(session_data).expect("proper base64");
    println!("{:?}", binary);
    println!(
        "{}",
        binary
            .iter()
            .map(|x| format!("{:x}", x))
            .collect::<Vec<_>>()
            .join(" ")
    );
    println!("{:?}", String::from_utf8_lossy(&binary));*/

    // Read https://www.lpalmieri.com/posts/2020-09-27-zero-to-production-4-are-we-observable-yet/
    LogTracer::init().expect("Failed to set logger");
    // NOTE This outputs JSON, but it can be made readable by using the executable `bunyan` and
    // pipe the result of `cargo run` to `bunyan`.
    /*
    let formatting_layer = BunyanFormattingLayer::new(
        "cubemania".into(),
        // Output the formatted spans to stdout.
        std::io::stdout,
    );*/

    // `set_global_default` can be used by applications to specify
    // what subscriber should be used to process spans.
    set_global_default(tracing_subscriber::fmt().finish()).expect("Failed to set subscriber");

    let db_url = env::var("DATABASE_URL").expect("DATABASE_URL must be set");

    let pool: PgPool = PgPoolOptions::new()
        .max_connections(5)
        .connect(&db_url)
        .await
        .expect("failed to create pool");

    let hmac_secret = env::var("HMAC_SECRET")
        .map_err(|_| "HMAC_SECRET not present")
        .unwrap();

    let app_state = AppState {
        pool: pool.clone(),
        jwt_secret: hmac_secret,
    };

    let _worker = actix_rt::spawn(runner(pool.clone()));

    HttpServer::new(move || {
        App::new()
            .wrap(TracingLogger::default())
            .app_data(web::Data::new(app_state.clone()))
            .route("/api/singles.csv", web::get().to(singles_csv))
            .route("/api/me", web::get().to(me_api))
            .route(
                "/api/max_singles_record",
                web::get().to(max_singles_record_api),
            )
            .route("/api/announcement", web::get().to(announcement_api))
            .route("/api/users", web::get().to(users_api))
    })
    .bind("0.0.0.0:8081")?
    .run()
    .await
}
