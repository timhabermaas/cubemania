use sqlx::postgres::PgPoolOptions;
use sqlx::PgPool;

#[derive(Clone)]
pub struct AppState {
    pub pool: PgPool,
    pub jwt_secret: String,
}

impl AppState {
    pub async fn new(db_url: String, jwt_secret: String) -> Self {
        let pool: PgPool = PgPoolOptions::new()
            .max_connections(5)
            .connect(&db_url)
            .await
            .expect("failed to create pool");

        AppState { pool, jwt_secret }
    }
}
