pub type Result<T> = core::result::Result<T, Error>;
#[derive(Debug)]
pub enum Error {
    // Unknown,
    Syntax(String),
    Runtime(String),
    System(String),
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Error::Syntax(err) => write!(f, "Syntax Err: {err}"),
            Error::Runtime(err) => write!(f, "Runtime Err: {err}"),
            Error::System(err) => write!(f, "System Err: {err}"),
        }
    }
}

impl std::error::Error for Error {}
