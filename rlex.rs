extern crate syntax;
extern crate rustc;

use rustc::session;
use rustc::session::config;

use syntax::diagnostics::registry;
use syntax::parse::lexer;
use syntax::parse::token;
use syntax::parse::token::{Lit, Token};
use syntax::parse;

use std::io;
use std::iter;

/* This is a simple standalone lexer based on rustc's lexer. The main
 * difference is a custom to_str function for tokens that prints in
 * the same format as the flex lexer in this project, so the results
 * between the two are easily comparable.
 */

fn token_to_string(tok: token::Token) -> String {
    match tok {
        Token::Literal(l, _) => match l {
            Lit::Byte(c) => {
                format!("Byte(b'{}')", c.as_str())
            },
            Lit::Char(c) => {
                format!("Char('{}')", c.as_str())
            },
            Lit::Integer(c) => {
                format!("Integer({})", c.as_str().to_string())
            },
            Lit::Float(c) => {
                format!("Float({})", c.as_str().to_string())
            },
            Lit::Str_(s) => {
                format!("Str(\"{}\")", token::get_name(s).get())
            },
            Lit::StrRaw(s, n) => {
                format!("StrRaw(r{delim}\"{string}\"{delim})",
                        delim=iter::repeat("#").take(n).collect::<String>(),
                        string=token::get_name(s))
            },
            Lit::Binary(s) => {
                format!("Binary(b\"{}\")", s.as_str())
            },
            Lit::BinaryRaw(s, n) => {
                format!("BinaryRaw(br{delim}\"{string}\"{delim})",
                        delim=iter::repeat("#").take(n).collect::<String>(),
                        string=token::get_name(s))                        
            },
        },
        Token::Ident(s, _) => {
            format!("Ident({})", token::get_ident(s).get().to_string())
        },
        Token::Lifetime(s) => {
            format!("Lifetime({})", token::get_ident(s).get().to_string())
        },
        Token::DocComment(s) => {
            format!("DocComment({})", token::get_name(s).get().to_string())
        },
        Token::Whitespace | Token::Comment => "".to_string(),
        Token::Shebang(s) => {
            format!("Shebang({})", token::get_name(s).get().to_string())
        },
        t => {
            format!("{:?}", t)
        },
    }
}

fn main() {
    let line = io::stdin().read_to_string().unwrap();

    let options = config::basic_options();
    let session = session::build_session(options, None,
                                         registry::Registry::new(&rustc::DIAGNOSTICS));
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
