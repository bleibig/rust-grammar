extern crate syntax;
extern crate rustc;

use rustc::driver::{session, config};

use syntax::diagnostics::registry;
use syntax::parse::lexer;
use syntax::parse::token;
use syntax::parse;

use std::io;

/* This is a simple standalone lexer based on rustc's lexer. The main
 * difference is a custom to_str function for tokens that prints in
 * the same format as the flex lexer in this project, so the results
 * between the two are easily comparable.
 */

fn token_to_string(tok: token::Token) -> String {
    match tok {
        token::LitByte(c) => {
            format!("LitByte({})", c.as_str())
        },
        token::LitInteger(c) => {
            format!("LitInteger({})", c.as_str().to_string())
        },
        token::LitFloat(c) => {
            format!("LitFloat({})", c.as_str().to_string())
        },
        token::LitStr(s) => {
          format!("LitStr(\"{}\")", token::get_name(s).get().escape_default())
        },
        token::LitStrRaw(s, n) => {
            format!("LitStrRaw(r{delim}\"{string}\"{delim})",
                     delim="#".repeat(n), string=token::get_name(s))
        },
        token::Ident(s, _) => {
            format!("Ident({})", token::get_ident(s).get().to_string())
        },
        token::Lifetime(s) => {
            format!("Lifetime({})", token::get_ident(s).get().to_string())
        },
        token::DocComment(s) => {
            format!("DocComment({})", token::get_name(s).get().to_string())
        },
        token::Whitespace | token::Comment => "".to_string(),
        t => {
            format!("{}", t)
        }
    }
}

fn main() {
    let line = io::stdin().read_to_string().unwrap();

    let options = config::basic_options();
    let session = session::build_session(options, None,
                                         registry::Registry::new(rustc::DIAGNOSTICS));
    let filemap = parse::string_to_filemap(&session.parse_sess,
                                           line,
                                           String::from_str("<n/a>"));
    let mut lexer = lexer::StringReader::new(session.diagnostic(), filemap);

    {
        use syntax::parse::lexer::Reader;
        while !lexer.is_eof() {
            let s = token_to_string(lexer.next_token().tok);
            if !s.is_empty() {
                println!("{}", s);
            }
        }
    }
}
