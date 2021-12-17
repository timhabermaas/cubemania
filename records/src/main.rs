extern crate jsonwebtoken as jwt;

mod app_state;
mod db;
mod error;
mod extractors;
mod handlers;
mod record_job;

use actix_web::{web, App, HttpServer};
use handlers::add_routes;
use record_job::runner;
use std::env;
use tracing::subscriber::set_global_default;
use tracing_actix_web::TracingLogger;
use tracing_log::LogTracer;

#[actix_web::main]
async fn main() -> std::io::Result<()> {
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

    let hmac_secret = env::var("HMAC_SECRET")
        .map_err(|_| "HMAC_SECRET not present")
        .unwrap();

    let app_state = app_state::AppState::new(db_url, hmac_secret).await;

    let _worker = actix_rt::spawn(runner(app_state.clone().pool));

    HttpServer::new(move || {
        App::new()
            .wrap(TracingLogger::default())
            .app_data(web::Data::new(app_state.clone()))
            .configure(add_routes)
    })
    .bind("0.0.0.0:8081")?
    .run()
    .await
}
