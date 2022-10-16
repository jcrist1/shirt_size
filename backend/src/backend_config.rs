use std::path::Path;

use config::{Config, ConfigError, File, FileFormat, FileSourceFile};
use serde::{Deserialize, Serialize};

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("Failed to load config: {0}")]
    Config(#[from] ConfigError),
}

#[derive(Debug, Serialize, Deserialize)]
pub struct HttpConfig {
    pub http_port: u16,
    pub host: String,
}

pub fn load<P: AsRef<Path>>(path: P) -> Result<HttpConfig, Error> {
    let conf =
        Config::builder().add_source(File::<FileSourceFile, FileFormat>::from(path.as_ref()));
    let conf = conf.build()?;
    Ok(conf.get::<HttpConfig>("http")?)
}
