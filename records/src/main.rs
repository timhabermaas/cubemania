extern crate jsonwebtoken as jwt;

mod db;
use actix_web::{
    error::ResponseError, http::StatusCode, web, App, HttpResponse, HttpServer, Responder,
};
use chrono::{DateTime, NaiveDateTime, Utc};
use jwt::{decode, Algorithm, DecodingKey, Validation};
use regex::Regex;
use serde::{Deserialize, Serialize};
use sqlx::postgres::PgPoolOptions;
use sqlx::prelude::Row;
use sqlx::PgPool;
use std::collections::HashMap;
use std::env;
use tracing::subscriber::set_global_default;
use tracing::{debug, error, info};
use tracing_actix_web::TracingLogger;
use tracing_bunyan_formatter::{BunyanFormattingLayer, JsonStorageLayer};
use tracing_log::LogTracer;
use tracing_subscriber::{layer::SubscriberExt, EnvFilter, Registry};

#[derive(Deserialize, Debug)]
struct SingleCsvClaim {
    download_csv_for: u32,
}

#[derive(Debug)]
struct Single {
    time: i32,
    dnf: bool,
    created_at: DateTime<Utc>,
    comment: String,
}

// Use String::from_utf8_lossy + writer to Vec<u8> for csv to get a UTF8 String. Lossy means
// unknown UTF8 symbols are replaced with some "unknown" character, but it never fails.
// See https://docs.rs/csv/1.1.5/csv/struct.Writer.html#example-1 for the entire roundtrip from
// data to CSV string.

/*
fn main() -> Result<(), Box<dyn error::Error>> {
    let mut client = Client::connect("postgres://postgres@db/cubemania_production", NoTls)?;

    let re = Regex::new(r"--- !ruby/struct:RecordCalculationJob\nuser_id: (\d+)\npuzzle_id: (\d+)")
        .unwrap();

    let foo = client.query("SELECT * FROM records", &[])?;
    println!("{:?}", foo);

    for row in client.query("SELECT handler from delayed_jobs", &[])? {
        let handler: String = row.get("handler");
        let capture = re.captures(&handler).unwrap();
        let user_id: i32 = capture
            .get(1)
            .unwrap()
            .as_str()
            .parse()
            .expect("user_id must be a string");
        let puzzle_id: i32 = capture
            .get(2)
            .unwrap()
            .as_str()
            .parse()
            .expect("puzzle_id must be a string");
        let singles = get_all_singles(&mut client, puzzle_id, user_id);
        println!("{:?}", singles);
    }
    Ok(())
}
*/

#[derive(Deserialize)]
struct SinglesQuery {
    user_id: u32,
    puzzle_id: u32,
    token: String,
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
    NotFound,
    DbError,
}

impl std::fmt::Display for AppError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match &self.error_type {
            ErrorType::JwtTokenInvalid => write!(f, "JWT token is invalid"),
            ErrorType::NotFound => write!(f, "Resource not found"),
            ErrorType::DbError => write!(f, "Db error"),
        }
    }
}

impl ResponseError for AppError {
    fn status_code(&self) -> StatusCode {
        match self.error_type {
            ErrorType::NotFound => StatusCode::NOT_FOUND,
            ErrorType::JwtTokenInvalid => StatusCode::UNAUTHORIZED,
            ErrorType::DbError => StatusCode::INTERNAL_SERVER_ERROR,
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

// Responder can be used for Result<T, E> as well as long as T: impl Responder and E: Into<Error>.
#[tracing::instrument(
    skip(q, app_state),
    fields(puzzle_id = q.puzzle_id, user_id = q.user_id)
)]
async fn singles_csv(
    web::Query(q): web::Query<SinglesQuery>,
    app_state: web::Data<AppState>,
) -> Result<impl Responder, AppError> {
    let token: &str = q.token.as_ref();
    let token_data = decode::<SingleCsvClaim>(
        token,
        &DecodingKey::from_secret(app_state.jwt_secret.as_ref()),
        &Validation::new(Algorithm::HS256),
    )
    .unwrap();

    if token_data.claims.download_csv_for != q.user_id {
        // TODO: error
        // 404 is correct, but also needs to be logged and get proper error response.
        info!(
            "claims to be {}, but wants files for {}",
            token_data.claims.download_csv_for, q.user_id
        );
        return Ok(HttpResponse::NotFound().body("csv file not found"));
    }

    // TODO: proper error handling
    let singles = db::fetch_singles(&app_state.pool, q.user_id, q.puzzle_id)
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
        .header(
            "Content-Disposition",
            "attachment; filename=\"singles.csv\"",
        )
        .header("Content-Type", "text/csv")
        //.header("Content-Type", "text/plain")
        .body(csv))
}

#[derive(Clone)]
struct AppState {
    pool: PgPool,
    jwt_secret: String,
}

#[actix_web::main]
async fn main() -> std::io::Result<()> {
    // Read https://www.lpalmieri.com/posts/2020-09-27-zero-to-production-4-are-we-observable-yet/
    LogTracer::init().expect("Failed to set logger");
    let env_filter = EnvFilter::try_from_default_env().unwrap_or_else(|_| EnvFilter::new("info"));
    // NOTE This outputs JSON, but it can be made readable by using the executable `bunyan` and
    // pipe the result of `cargo run` to `bunyan`.
    let formatting_layer = BunyanFormattingLayer::new(
        "cubemania".into(),
        // Output the formatted spans to stdout.
        std::io::stdout,
    );

    // The `with` method is provided by `SubscriberExt`, an extension
    // trait for `Subscriber` exposed by `tracing_subscriber`
    let subscriber = Registry::default()
        .with(env_filter)
        .with(JsonStorageLayer)
        .with(formatting_layer);
    // `set_global_default` can be used by applications to specify
    // what subscriber should be used to process spans.
    set_global_default(subscriber).expect("Failed to set subscriber");

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
        pool,
        jwt_secret: hmac_secret,
    };

    HttpServer::new(move || {
        App::new()
            .wrap(TracingLogger)
            .data(app_state.clone())
            .route("/api/singles.csv", web::get().to(singles_csv))
    })
    .bind("0.0.0.0:8081")?
    .run()
    .await
}
