use std::fmt::Write;

static mut TRACE_LEVEL: i32 = 0;

const TRACE_IDENT_PLACEHOLDER: &str = "\t";

fn ident_level() -> String {
    unsafe {
        TRACE_IDENT_PLACEHOLDER.repeat((TRACE_LEVEL - 1) as usize)
    }
}

fn trace_print(fs: &str) {
    print!("{}{}", ident_level(), fs);
}

fn inc_ident() {
    unsafe {
        TRACE_LEVEL += 1;
    }
}

fn dec_ident() {
    unsafe {
        TRACE_LEVEL -= 1;
    }
}

pub fn trace(msg: &str) -> &str {
    inc_ident();
    trace_print(&format!("BEGIN {}", msg));
    msg
}

pub fn untrace(msg: &str) {
    trace_print(&format!("END {}", msg));
    dec_ident();
}
