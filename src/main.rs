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

fn main() -> Result<(), anyhow::Error> {
    let args = Args::parse();
    let input = read_to_string(&args.input)?;
    let tokens = Lexer::new(args.input.clone(), input).tokenize()?;
    let ast = Parser::new(tokens).parse()?;
    let output = Generator::new().generate(ast);
    std::fs::write(&args.output, output)?;

    Ok(())
}
