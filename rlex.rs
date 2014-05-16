extern crate syntax;
extern crate rustc;

use syntax::parse;
use syntax::parse::lexer;
use syntax::parse::token;
use syntax::ast_util;
use rustc::driver::{session, config};

use std::io;

/* This is a simple standalone lexer based on rustc's lexer. The main
 * difference is a custom to_str function for tokens that prints in
 * the same format as the flex lexer in this project, so the results
 * between the two are easily comparable.
 */

fn token_to_str(tok: token::Token) -> ~str {
    match tok {
        token::LIT_CHAR(_c) => {
            format!("LIT_CHAR(char)")
        },
        token::LIT_UINT(v, s) => {
            format!("LIT_UINT({}{})", v, s.to_str().replace("uint", "u").replace("int", "i"))
        },
        token::LIT_INT(v, s) => {
            format!("LIT_INT({}{})", v, s.to_str().replace("uint", "u").replace("int", "i"))
        },
        token::LIT_INT_UNSUFFIXED(v) => {
            format!("LIT_INT_UNSUFFIXED({}i64)", v as i64)
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
            format!("LIT_FLOAT_UNSUFFIXED({})", body)
        },
        token::LIT_STR(_s) => {
            /*
            format!("LIT_STR(\"{}\")", token::get_ident(s).get().chars().fold(StrBuf::new(),
                |mut result, c| {
                    match c {
                        '\n' => result.push_str("\\n"),
                        '\t' => result.push_str("\\t"),
                        '\r' => result.push_str("\\r"),
                        '\\' => result.push_str("\\\\"),
                        '"'  => result.push_str("\\\""),
                        ' '..'~' => result.push_char(c),
                        _    => c.escape_unicode(|c| result.push_char(if c != 'u' { c.to_uppercase() } else { 'u' }))
                    }
                    result
                }))
            */
            format!("LIT_STR(contents)")
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
    let line = io::stdin().read_to_str().unwrap();

    let options = config::basic_options();
    let session = session::build_session(options, None);
    let filemap = parse::string_to_filemap(&session.parse_sess,
                                           StrBuf::from_owned_str(line),
                                           StrBuf::from_str("<n/a>"));
    let mut lexer = lexer::new_string_reader(session.diagnostic(), filemap);

    {
        use syntax::parse::lexer::Reader;
        while !lexer.is_eof() {
            println!("{}", token_to_str(lexer.next_token().tok));
        }
    }
}
