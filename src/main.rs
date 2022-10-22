use clap::Parser as ClapParser;
use std::fs::read_to_string;
use tocc::generator::Generator;
use tocc::lexer::Lexer;
use tocc::parser::Parser;

#[derive(ClapParser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    input: String,
    #[arg(short, long)]
    output: String,
}

fn main() {
    let args = Args::parse();
    let input = read_to_string(&args.input).unwrap();
    let tokens = Lexer::new(args.input.clone(), input).tokenize().unwrap();
    let ast = Parser::new(tokens).expr().unwrap();
    let mut generator = Generator::new();
    generator.expr(ast);
    let mut output = r"
    .intel_syntax noprefix
    .globl main
    main:
    "
    .to_string();
    output.push_str(&generator.output());
    output.push_str(
        "
    pop rax
    ret
    ",
    );
    std::fs::write(&args.output, output).unwrap();
}
