use clap::Parser as ClapParser;
use std::fs::read_to_string;
use tocc::generator;
use tocc::lexer::Lexer;
use tocc::parser::Parser;
use tocc::static_analysis;
use tocc::tac;
use tocc::tac_generator;
#[derive(ClapParser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    input: String,
    #[arg(short, long)]
    output: String,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args = Args::parse();
    let input = read_to_string(&args.input)?;
    let tokens = Lexer::new(args.input.clone(), input).tokenize()?;
    let clang = Parser::new(tokens).parse()?;
    static_analysis(&clang)?;
    let mut tac = tac_generator::generate(clang)?;
    tac::optimize(&mut tac);
    let asm = generator::generate(tac);
    std::fs::write(&args.output, asm)?;

    Ok(())
}
