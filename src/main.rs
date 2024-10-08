use std::env;
use std::fs;
use std::io::{self, Write};
use std::path::PathBuf;
use clap::command;
use clap::Parser;
use clap::Subcommand;
use codecrafters_interpreter as ci;
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
    Parse {
        filename : PathBuf
    },
    Run {
        filename : PathBuf
    }
}


fn main() -> miette::Result<()>{
    let args = Args::parse();
    let mut flagToExit = false;
    match args.command {
        Commands::Tokenize { filename } => {
            // You can use print statements as follows for debugging, they'll be visible when running tests.
            writeln!(io::stderr(), "Logs from your program will appear here!").unwrap();

            let file_contents = fs::read_to_string(&filename)
                                                        .into_diagnostic()
                                                        .wrap_err_with(|| format!("reading '{}' failed" , filename.display()))?;

            let lexer = ci::Lexer::new(&file_contents);

            for token in lexer {
                match token {
                    Ok(t) => {
                        println!("{t}");
                    },
                    Err(e) => {
                        eprintln!("{:?}", e);
                        if let Some(un_error) = e.downcast_ref::<ci::lex::InternalTokenError>() {
                            flagToExit = true;
                            eprintln!("[line {}] Error: Unexpected character: {}",un_error.line(), un_error.token);
                            // std::process::exit(65);
                        }
                        else if let Some(unterminated_str) = e.downcast_ref::<ci::lex::StringTerminationError>() {
                            flagToExit = true;
                            eprintln!("[line {}] Error: Unterminated string.",unterminated_str.line());
                            // std::process::exit(65);
                        }
                        continue;
                        // return Err(e);
                    }
                }
                
            }
            println!("EOF  null");
        },
        Commands::Parse { filename } => {
            let file_contents = fs::read_to_string(&filename)
                .into_diagnostic()
                .wrap_err_with(|| format!("reading {} failed", filename.display()))?;

            let mut parser = ci::Parser::new(&file_contents);

            match parser.parse_expression() {
                Ok(tt) => println!("{tt}"),
                Err(e) => {
                    eprintln!("{e:?}");
                    std::process::exit(65);
                }
            }
            // println!("{}", parser.parse_expression().unwrap());
        },
        Commands::Run { filename } => {
            let file_contents = fs::read_to_string(&filename)
                .into_diagnostic()
                .wrap_err_with(|| format!("reading {} failed", filename.display()))?;

            let mut parser = ci::Parser::new(&file_contents);
            println!("{}", parser.parse().unwrap());
        }
    }

    if flagToExit {
        std::process::exit(65);
    }

    Ok(())
}
