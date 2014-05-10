extern crate syntax;
extern crate rustc;

use syntax::parse;
use syntax::parse::lexer;
use syntax::parse::lexer::Reader;
use syntax::parse::token;
use syntax::ast_util;
use rustc::driver::driver;
use rustc::driver::session;

use std::io;

/* This is a simple standalone lexer based on rustc's lexer. The main
 * difference is a custom to_str function for tokens that prints in
 * the same format as the flex lexer in this project, so the results
 * between the two are easily comparable.
 */

fn token_to_str(tok: token::Token) -> ~str {
    match tok {
        token::LIT_CHAR(c) => {
            let mut res = StrBuf::from_str("'");
            c.escape_default(|c| {
                res.push_char(c);
            });
            res.push_char('\'');
            format!("LIT_CHAR({})", res)
        },
        token::LIT_FLOAT(s, t) => {
            let mut body = StrBuf::from_str(token::get_ident(s).get().to_str());
            if body.as_slice().ends_with(".") {
                body.push_char('0');  // `10.f` is not a float literal
            }            
            format!("LIT_FLOAT({}{})", body, ast_util::float_ty_to_str(t))
        },
        token::LIT_FLOAT_UNSUFFIXED(s) => {
            let mut body = StrBuf::from_str(token::get_ident(s).get().to_str());
            if body.as_slice().ends_with(".") {
                body.push_char('0');  // `10.f` is not a float literal
            }
            format!("LIT_FLOAT({})", body)
        },
        token::LIT_STR(s) => {
            format!("LIT_STR(\"{}\")", token::get_ident(s).get().escape_default())
        },
        token::LIT_STR_RAW(s, n) => {
            format!("LIT_STR_RAW(r{delim}\"{string}\"{delim})",
                     delim="#".repeat(n), string=token::get_ident(s))
        },
        token::IDENT(s, _) => {
            format!("IDENT({})", token::get_ident(s).get().to_str())
        },
        token::LIFETIME(s) => {
            format!("LIFETIME('{})", token::get_ident(s).get().to_str())
        },
        token::DOC_COMMENT(s) => {
            format!("DOC_COMMENT({})", token::get_ident(s).get().to_str())
        },
        t => {
            format!("{:?}", t)
        }
    }
}

fn main() {
    for maybe_line in io::stdin().lines() {
        match maybe_line {
            Ok(line) => {
                let options = session::basic_options();
                let session = driver::build_session(options, None);
                let filemap = parse::string_to_filemap(&session.parse_sess,
                                                       StrBuf::from_owned_str(line),
                                                       StrBuf::from_str("<n/a>"));
                let mut lexer = lexer::new_string_reader(session.diagnostic(), filemap);

                while !lexer.is_eof() {
                    println!("{}", token_to_str(lexer.next_token().tok));
                }
            },
            Err(ioerr) => {
                fail!("I/O Error: {}", ioerr.desc);
            }
        }
    }
}
