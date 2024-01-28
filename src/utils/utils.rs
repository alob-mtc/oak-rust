use std::io::{self, Write};

use crate::error::error::Error;

static ANSI_RESET: &str = "[0;0m";
static ANSI_BLUE: &str = "[34;22m";
static ANSI_RED: &str = "[31;22m";
static ANSI_BLUE_BOLD: &str = "[34;1m";
static ANSI_GREEN_BOLD: &str = "[32;1m";
static ANSI_RED_BOLD: &str = "[31;1m";

pub fn log_interactive(args: &str) {
    print!("{ANSI_GREEN_BOLD}{args}{ANSI_RESET}");
    io::stdout().flush().unwrap();
}

pub fn log_debug(args: &str) {
    print!(
        "{}debug: {}{}{}",
        ANSI_BLUE_BOLD, ANSI_BLUE, args, ANSI_RESET
    );
    io::stdout().flush().unwrap();
}

pub fn log_safe_err(reason: &Error) {
    eprintln!("{}{}: {}{}", ANSI_RED_BOLD, reason, ANSI_RED, ANSI_RESET);
    io::stderr().flush().unwrap()
}
