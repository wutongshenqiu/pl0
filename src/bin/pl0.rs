use std::{fs, path::PathBuf};

use clap::Parser;
use pl0::{ASTNodeEval, ASTNodeGen, Context, Intepreter, Parser as Pl0Parser, Pl0Error, Result};

#[derive(Parser)]
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
    #[arg(long, action)]
    show: bool,
}

fn main() -> Result<()> {
    let args = Pl0Args::parse();
    let src = fs::read_to_string(args.source)?;
    let ast = Pl0Parser::parse_str(&src).unwrap();

    match args.mode.as_str() {
        "eval" => {
            let mut context = Context::default();
            if ast.eval(&mut context)?.is_some() {
                Err(Pl0Error::InvalidAST)
            } else {
                Ok(())
            }
        }
        "ir" => {
            let mut buf = Vec::new();
            ast.gen(&mut buf)?;
            if args.show {
                for (i, ir) in buf.iter().enumerate() {
                    println!("{}: {:?}", i, ir);
                }
            }
            let mut context = Context::default();
            Intepreter::ir_eval(&buf, &mut context)
        }
        _ => Err(Pl0Error::UnsupportedMode),
    }
}
