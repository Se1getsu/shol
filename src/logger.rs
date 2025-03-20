#[macro_export]
macro_rules! log {
    ($logger:expr, $($arg:tt)*) => {
        $logger.log(format!($($arg)*));
    };
}

pub struct Logger {
    enabled: bool,
}

impl Logger {
    pub fn new(enabled: bool) -> Self {
        Self { enabled }
    }

    pub fn log(&self, msg: String) {
        if self.enabled {
            println!("{}", msg);
        }
    }
}
