mod ast;
pub mod parser;
struct ScopeCall<F: FnMut()> {
    c: Option<F>,
}

impl<F: FnMut()> Drop for ScopeCall<F> {
    fn drop(&mut self) {
        self.c.take().unwrap()()
    }
}
