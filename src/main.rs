use lexer::tokenizer;
use parser::parser::Parser;

mod error;
mod lexer;
mod parser;
mod runtime;

fn main() {
    let source_string = r"
std := import('std')
fmt := import('fmt')
http := import('http')

server := http.Server()
with server.route('/hello/:name') fn(params) {
    fn(req, end) if req.method {
        'GET' -> end({
            status: 200
            body: fmt.format('Hello, {{ 0 }}!'
                std.default(params.name, 'World'))
        })
        _ -> end(http.MethodNotAllowed)
    }
}
server.start(9999)
    ";
    let t = tokenizer::Tokenizer::new(source_string.to_string());
    let tokens = t.tokenize();

    println!(" tokens === {:?}", tokens);

    let p = Parser::new(tokens);
    let nodes = p.parse().expect("no error");

    println!(" node === {:?}", nodes)
}
