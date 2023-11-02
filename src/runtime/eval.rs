use core::fmt;

// internal values
const EMPTY: Value = Value::Empty;
const NULL: Value = Value::Null;
const TRUE: Value = Value::Bool(true);
const FALSE: Value = Value::Bool(false);

// return the max of two slices
fn max_len(a: &[u8], b: &[u8]) -> usize {
    if a.len() < b.len() {
        b.len()
    } else {
        a.len()
    }
}

// zero-extend a slice of bytes to given length
fn zero_extends(s: &[u8], max: usize) -> Vec<u8> {
    let mut extended = vec![0; max];
    let len = std::cmp::min(s.len(), max);
    extended[..len].copy_from_slice(&s[..len]);
    extended
}

pub enum Value {
    Empty,
    Null,
    String(String),
    Int(i64),
    Float(f64),
    Bool(bool),
    Atom(String),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Empty => write!(f, "_"),
            Value::Null => write!(f, "?"),
            Value::String(v) => write!(f, "{v}"),
            Value::Int(v) => write!(f, "{v}"),
            Value::Float(v) => write!(f, "{:.*}", 64, v),
            Value::Bool(v) => write!(f, "{v}"),
            Value::Atom(v) => write!(f, ":{v}"),
        }
    }
}

impl Value {
    pub fn eq(&self, val: &Value) -> bool {
        match self {
            Value::Empty => true,
            Value::Null => match val {
                Value::Empty | Value::Null => true,
                _ => false,
            },
            Value::String(v) => match val {
                Value::Empty => true,
                Value::String(w) => v == w,
                _ => false,
            },
            Value::Int(v) => match val {
                Value::Empty => true,
                Value::Int(w) => v == w,
                Value::Float(w) => *v as f64 == *w,
                _ => false,
            },
            Value::Float(v) => match val {
                Value::Empty => true,
                Value::Float(w) => v == w,
                Value::Int(w) => *v == *w as f64,
                _ => false,
            },
            Value::Bool(v) => match val {
                Value::Empty => true,
                Value::Bool(w) => v == w,
                _ => false,
            },
            Value::Atom(v) => match val {
                Value::Empty => true,
                Value::Atom(w) => v == w,
                _ => false,
            },
        }
    }
}
