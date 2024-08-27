use std::env;
use std::fs;
use std::io::{self, Write};
use std::path::PathBuf;
use clap::command;
use clap::Parser;
use clap::Subcommand;
use codecrafters_interpreter::*;
use miette::Context;
use miette::IntoDiagnostic;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand,Debug)]
enum Commands {
    /// does testing things
    Tokenize {
        filename: PathBuf,
    },
}


fn main() -> miette::Result<()>{
    let args = Args::parse();

    match args.command {
        Commands::Tokenize { filename } => {
            // You can use print statements as follows for debugging, they'll be visible when running tests.
            writeln!(io::stderr(), "Logs from your program will appear here!").unwrap();

            let file_contents = fs::read_to_string(&filename)
                                                        .into_diagnostic()
                                                        .wrap_err_with(|| format!("reading '{}' failed" , filename.display()))?;

            let lexer = Lexer::new(&file_contents);

            for token in lexer {
                let token = token?;
                println!("{token}");
            }
            println!("EOF  null");
        }
    }

    Ok(())
}
