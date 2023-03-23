use std::{fs, path::PathBuf};

use clap::Parser;
use pl0::{ASTNodeEval, EvalContext, Lexer, Parser as Pl0Parser, Pl0Error, Result};

#[derive(Parser, Debug)]
#[command(author, version, about)]
struct Pl0Args {
    #[arg(long, short, help = "path of pl0 source file")]
    source: PathBuf,
    #[arg(
        long,
        short,
        default_value = "eval",
        help = "mode of execution, support eval or ir"
    )]
    mode: String,
}

fn main() -> Result<()> {
    let args = Pl0Args::parse();
    let src = fs::read_to_string(args.source)?;

    let lexer = Lexer::new(&src);
    let parser = Pl0Parser::new(lexer);
    let ast = parser.parse()?;

    match args.mode.as_str() {
        "eval" => {
            let mut context = EvalContext::default();
            if ast.eval(&mut context)?.is_some() {
                Err(Pl0Error::InvalidAST)
            } else {
                Ok(())
            }
        }
        "ir" => {
            unimplemented!()
        }
        _ => Err(Pl0Error::UnsupportedMode),
    }
}
