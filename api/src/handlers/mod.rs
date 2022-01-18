pub mod announcement;
pub mod puzzle;
pub mod record;
pub mod single;
pub mod user;

use actix_web::web::{get, put, ServiceConfig};

pub fn add_routes(cfg: &mut ServiceConfig) {
    cfg.route(
        "/api/singles.csv",
        get().to(crate::handlers::single::singles_csv),
    )
    .route("/api/me", get().to(crate::handlers::user::me_api))
    .route(
        "/api/max_singles_record",
        get().to(crate::handlers::single::max_singles_count_api),
    )
    .route(
        "/api/announcement",
        get().to(crate::handlers::announcement::announcement_api),
    )
    .route("/api/users", get().to(crate::handlers::user::users_api))
    .route(
        "/api/users/{user_slug}/block",
        put().to(crate::handlers::user::user_block_api),
    )
    .route(
        "/api/puzzles",
        get().to(crate::handlers::puzzle::puzzles_api),
    )
    .route(
        "/api/records",
        get().to(crate::handlers::record::records_api),
    )
    .route(
        "/api/records/{record_id}",
        get().to(crate::handlers::record::record_api),
    );
}
