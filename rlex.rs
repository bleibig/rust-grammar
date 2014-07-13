extern crate syntax;
extern crate rustc;
extern crate debug;

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
        token::LIT_CHAR(c) => {
            format!("LIT_CHAR({})", c.as_str())
        },
        token::LIT_INTEGER(c) => {
            format!("LIT_INTEGER({})", c.as_str().to_string())
        },
        token::LIT_FLOAT(c) => {
            format!("LIT_FLOAT({})", c.as_str().to_string())
        },
        token::LIT_STR(s) => {
          format!("LIT_STR(\"{}\")", token::get_name(s).get().escape_default())
        },
        token::LIT_STR_RAW(s, n) => {
            format!("LIT_STR_RAW(r{delim}\"{string}\"{delim})",
                     delim="#".repeat(n), string=token::get_name(s))
        },
        token::IDENT(s, _) => {
            format!("IDENT({})", token::get_ident(s).get().to_string())
        },
        token::LIFETIME(s) => {
            format!("LIFETIME({})", token::get_ident(s).get().to_string())
        },
        token::DOC_COMMENT(s) => {
            format!("DOC_COMMENT({})", token::get_name(s).get().to_string())
        },
        token::WS | token::COMMENT => "".to_string(),
        t => {
            format!("{:?}", t)
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
