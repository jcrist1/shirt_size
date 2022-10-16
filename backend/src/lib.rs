use std::{env::VarError, net::AddrParseError};

#[cfg(not(target_arch = "wasm32"))]
use axum::{http::StatusCode, response::IntoResponse};
#[cfg(not(target_arch = "wasm32"))]
use tokio::sync::broadcast::error::SendError;

use futures_util::sink::SinkErrInto;
pub mod backend_config;
pub mod shirt_state;

#[cfg(not(target_arch = "wasm32"))]
pub mod auth;

#[cfg(not(target_arch = "wasm32"))]
use auth::AuthError;

const DEFAULT_CONFIG_PATH: &str = "config/config.toml";
#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("r_ecipe_s failed to bind server with io error: {0}")]
    IO(#[from] std::io::Error),
    #[error("r_ecipe_s failed to load config from {DEFAULT_CONFIG_PATH}, Config Error {0}")]
    Confg(#[from] backend_config::Error),
    #[error("Failed to parse address from connection config: {0}")]
    AddrParse(#[from] AddrParseError),
    #[error("{0:?}")]
    Message(String),
    #[error("Failed to parse environment variable: {0:?}")]
    Var(#[from] VarError),
    #[error("serde error: {0:?}")]
    Sender(#[from] serde_json::Error),
    #[cfg(not(target_arch = "wasm32"))]
    #[error("web socket send error: {0}")]
    WsSend(#[from] axum::Error),
    #[cfg(not(target_arch = "wasm32"))]
    #[error("failed to send update: {0:?}")]
    Snd(#[from] SendError<()>),
    #[cfg(not(target_arch = "wasm32"))]
    #[error("{0}")]
    Auth(#[from] AuthError),
}
pub type Result<T> = std::result::Result<T, Error>;

#[cfg(not(target_arch = "wasm32"))]
impl IntoResponse for Error {
    fn into_response(self) -> axum::response::Response {
        match self {
            Self::Auth(auth) => auth.into_response(),
            _ => {
                let code = StatusCode::INTERNAL_SERVER_ERROR;
                (code, format!("{self}")).into_response()
            }
        }
    }
}
