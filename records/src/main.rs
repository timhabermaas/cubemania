use actix_web::{get, post, web, App, HttpResponse, HttpServer, Responder};
use chrono::TimeZone;
use chrono::{DateTime, NaiveDateTime, Utc};
use regex::Regex;
use serde::{Deserialize, Serialize};
use sqlx::postgres::PgPoolOptions;
use sqlx::PgPool;
use tracing::subscriber::set_global_default;
use tracing::{debug, info};
use tracing_actix_web::TracingLogger;
use tracing_bunyan_formatter::{BunyanFormattingLayer, JsonStorageLayer};
use tracing_log::LogTracer;
use tracing_subscriber::{layer::SubscriberExt, EnvFilter, Registry};

#[derive(Debug)]
struct Single {
    time: i32,
    dnf: bool,
    created_at: DateTime<Utc>,
    comment: String,
}

#[tracing::instrument(skip(pool))]
async fn fetch_singles(
    pool: &PgPool,
    user_id: u32,
    puzzle_id: u32,
) -> Result<Vec<SingleResult>, sqlx::Error> {
    sqlx::query_as(
        "SELECT id, comment, time, created_at FROM singles WHERE puzzle_id = $1 AND user_id = $2 ORDER BY created_at",
    )
    .bind(puzzle_id)
    .bind(user_id)
    .fetch_all(pool )
    .await
}
/*
fn get_all_singles(
    client: &mut Client,
    puzzle_id: u32,
    user_id: u32,
) -> Result<Vec<Single>, Box<dyn error::Error>> {
    let rows = client.query("SELECT id, time, penalty, created_at FROM singles WHERE user_id = $1 AND puzzle_id = $2 ORDER BY created_at", &[&user_id, &puzzle_id])?;
    // TODO: Is this inefficient? We create an iterator and create a vec again and later iterate
    // over that Vec.
    Ok(rows
        .iter()
        .map(|row| Single {
            time: row.get("time"),
            dnf: false,
            created_at: Utc.from_local_datetime(&row.get("created_at")).unwrap(),
            comment: row.get("comment"),
        })
        .collect())
}
*/

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

#[get("/")]
async fn hello() -> impl Responder {
    HttpResponse::Ok().body("Hello world!")
}

#[post("/echo")]
async fn echo(req_body: String) -> impl Responder {
    HttpResponse::Ok().body(req_body)
}

async fn manual_hello() -> impl Responder {
    HttpResponse::Ok().body("Hey there!")
}

#[derive(Deserialize)]
struct SinglesQuery {
    user_id: u32,
    puzzle_id: u32,
}

#[derive(Serialize)]
struct CsvRow<'a> {
    comment: &'a str,
    time: i32,
    created_at: NaiveDateTime,
}

#[tracing::instrument(skip(singles))]
fn create_csv_from_singles(singles: &[SingleResult]) -> Result<String, csv::Error> {
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
            })?;
        }

        wtr.flush()?;
    }
    info!("CSV file successfully created, bytes={}", buffer.len());
    Ok(String::from_utf8_lossy(&buffer).into_owned())
}

// TODO: We can probably use a stream here? Does actix support some kind of streamed response?

#[tracing::instrument(
    skip(q, pool),
    fields(puzzle_id = q.puzzle_id, user_id = q.user_id)
)]
async fn singles_csv(
    web::Query(q): web::Query<SinglesQuery>,
    pool: web::Data<PgPool>,
) -> impl Responder {
    // TODO: proper error handling
    let singles = fetch_singles(pool.as_ref(), q.user_id, q.puzzle_id)
        .await
        .expect("query failed");

    debug!("{} singles fetched", singles.len());

    // TODO: Handle failures by writing to error!
    let csv = create_csv_from_singles(&singles).expect("should never fail");

    HttpResponse::Ok()
        /*
        .header(
            "Content-Disposition",
            "attachment; filename=\"singles.csv\"",
        )*/
        //.header("Content-Type", "text/csv")
        .header("Content-Type", "text/plain")
        .body(csv)
}

#[derive(sqlx::FromRow, Debug)]
struct SingleResult {
    id: i32,
    time: i32,
    comment: Option<String>,
    created_at: NaiveDateTime,
}

#[actix_web::main]
async fn main() -> std::io::Result<()> {
    //std::env::set_var("RUST_LOG", "main=info,actix_web=info");
    //env_logger::init();

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

    // TODO: Make logging to file work.

    // Just a useless file experiment.
    let file_appender = tracing_appender::rolling::daily("./logs", "server.log");
    let (non_blocking, _guard) = tracing_appender::non_blocking(file_appender);
    // Can probably be combined with BunyanFormattingLayer to achieve a bunyan format written to
    // files.
    let file_appender_layer = tracing_subscriber::fmt::layer().with_writer(non_blocking);

    // The `with` method is provided by `SubscriberExt`, an extension
    // trait for `Subscriber` exposed by `tracing_subscriber`
    let subscriber = Registry::default()
        .with(env_filter)
        .with(JsonStorageLayer)
        .with(formatting_layer)
        .with(file_appender_layer);
    // `set_global_default` can be used by applications to specify
    // what subscriber should be used to process spans.
    set_global_default(subscriber).expect("Failed to set subscriber");

    let pool: PgPool = PgPoolOptions::new()
        .max_connections(5)
        .connect("postgres://postgres@localhost:5432/cubemania_production")
        .await
        .expect("failed to create pool");

    let result: Vec<SingleResult> = sqlx::query_as(
        "SELECT id, comment, time, created_at FROM singles WHERE puzzle_id = $1 AND user_id = $2",
    )
    .bind(1)
    .bind(1)
    .fetch_all(&pool)
    .await
    .expect("query failed");

    info!("{:?}", result);

    HttpServer::new(move || {
        App::new()
            .wrap(TracingLogger)
            .data(pool.clone())
            .service(hello)
            .service(echo)
            .route("/hey", web::get().to(manual_hello))
            .route("/api/singles.csv", web::get().to(singles_csv))
    })
    .bind("127.0.0.1:8081")?
    .run()
    .await
}
