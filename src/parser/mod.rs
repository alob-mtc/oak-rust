mod ast;
mod parser;
struct ScopeCall<F: FnOnce()> {
    c: Option<F>,
}

impl<F: FnOnce()> Drop for ScopeCall<F> {
    fn drop(&mut self) {
        self.c.take().unwrap()()
    }
}
