use clap::Parser;
use std::process::ExitCode;
mod expression;
mod interpretter;
mod intrinsics;
mod lexer;
mod parser;
mod runner;
mod span;
mod token;
mod typechecker;
mod types;
mod value;

#[derive(Parser, Debug)]
#[command(version, about)]
struct Args {
    /// File name to run
    #[arg()]
    program_name: Option<String>,
    /// Print parse tree after parsing (interactive only)
    #[arg(short, long)]
    tree_print: bool,
}

fn main() -> ExitCode {
    let args = Args::parse();
    // let _program = args.next().expect("no program name");
    let val = args.program_name.map_or_else(
        || runner::repl(args.tree_print),
        |program_file| runner::read_and_run(&program_file).map(|_| ()),
    );
    match val {
        Ok(()) => ExitCode::SUCCESS,
        Err(e) => {
            println!("{e}");
            ExitCode::FAILURE
        }
    }
}
