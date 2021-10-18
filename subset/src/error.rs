use std::error::Error as StdError;
use std::fmt;

#[derive(Debug)]
pub enum Error {
    Parcom(parcom::Error),
}

impl From<parcom::Error> for Error {
    fn from(err: parcom::Error) -> Self {
        Self::Parcom(err)
    }
}

impl StdError for Error {
    fn source(&self) -> Option<&(dyn StdError + 'static)> {
        match self {
            Self::Parcom(err) => Some(err),
        }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Parcom(p) => {
                write!(f, "From parcom {}", p)
            }
        }
    }
}
