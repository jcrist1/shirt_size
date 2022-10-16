use axum::http::StatusCode;
use axum::Router;
use futures::future::abortable;
use std::net::SocketAddr;

use axum::routing::get_service;
use axum_sessions::{async_session::MemoryStore, SessionLayer};
use shirt_size_server::{backend_config, Error, Result};
use std::env;
use tower_http::{
    services::{ServeDir, ServeFile},
    trace::TraceLayer,
};
use tracing_subscriber::{layer::SubscriberExt, util::SubscriberInitExt};

#[cfg(not(target_arch = "wasm32"))]
#[tokio::main]
async fn main() -> Result<()> {
    use axum_sessions::extractors::WritableSession;

    tracing_subscriber::registry()
        .with(tracing_subscriber::EnvFilter::new(
            std::env::var("RUST_LOG")
                .unwrap_or_else(|_| "example_static_file_server=debug,tower_http=debug".into()),
        ))
        .with(tracing_subscriber::fmt::layer())
        .init();
    let http_config = backend_config::load("config/config.toml")?;

    env::set_current_dir("../frontend")?;

    let api_key = env::var("API_KEY")?;

    env::set_var("RUST_LOG", "axum=info,sqlx=warn");
    let store = MemoryStore::new();
    let secret = api_key.as_bytes();
    let session_layer = SessionLayer::new(store, secret);
    let host_port = http_config.http_port;
    let shirt_size_service =
        shirt_size_server::shirt_state::shirt_service::ShirtSizeService::new(&api_key);

    let sock_addr = SocketAddr::new(http_config.host.parse()?, http_config.http_port);
    let app = Router::new()
        .nest(
            "/api/v1",
            Router::new().nest(
                "/shirt-size",
                shirt_size_service.routes(session_layer.clone()),
            ),
        )
        .nest(
            "/static",
            get_service(ServeDir::new("static")).handle_error(|error: std::io::Error| async move {
                (
                    StatusCode::INTERNAL_SERVER_ERROR,
                    format!("Unhandled internal error: {}", error),
                )
            }),
        )
        .nest(
            "/index.html",
            get_service(ServeFile::new("index.html")).handle_error(
                |error: std::io::Error| async move {
                    (
                        StatusCode::INTERNAL_SERVER_ERROR,
                        format!("Unhandled internal error: {}", error),
                    )
                },
            ),
        )
        .fallback(Router::new().nest(
            "/",
            get_service(ServeDir::new("dist")).handle_error(|error: std::io::Error| async move {
                (
                    StatusCode::INTERNAL_SERVER_ERROR,
                    format!("Unhandled internal error: {}", error),
                )
            }),
        ))
        .layer(TraceLayer::new_for_http())
        .layer(session_layer);
    tracing::info!("Successfully bound server to {}", host_port);
    let http_server = axum::Server::bind(&sock_addr).serve(app.into_make_service());

    let tasks = tokio::spawn(async move { http_server.await });
    let (fut, handle) = abortable(tasks);
    let hook = std::panic::take_hook();
    std::panic::set_hook(Box::new(move |info| {
        handle.abort();
        hook(info);
    }));
    let res = fut
        .await
        .map_err(|_err| Error::Message("Failed to run server tasks".into()))?;
    let res = res.map_err(|err| Error::Message(format!("Error in server: {err:?}")))?;

    res.map_err(|err| Error::Message(format!("Error in server: {err:?}")))?;
    Ok(())
}
