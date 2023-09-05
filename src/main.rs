mod lexer;
mod parser;

fn main() {
    let source_string = String::from(
        r#"
        // fibonacci sequence

{
	println: println
	range: range
	map: map
} := import('std')

fn fibNaive(n) if n <= 1 {
	true -> 1
	_ -> fibNaive(n - 1) + fibNaive(n - 2)
}

fn fibFast(n) {
	fn sub(n, a, b) if n <= 1 {
		true -> b
		_ -> sub(n - 1, b, a + b)
	}
	sub(n, 1, 1)
}

println('naive:', range(20) |> map(fibNaive))
println('fast:', range(20) |> map(fibFast))

    "#,
    );
    let mut tokenizer = lexer::tokenizer::Tokenizer::new(source_string);
    let tokens = tokenizer.tokenize();

    println!("{tokens:?}");
}
