#![allow(unused_must_use)]

extern crate syntax;
extern crate rustc;
extern crate serialize;
extern crate getopts;

use getopts::{optflag,getopts};

use std::io;
use std::os;
use std::io::Reader;
use std::string::String;
use serialize::json;
use serialize::json::{Json, List, String, Object};
use syntax::diagnostics::registry;

use rustc::driver::{driver, session, config};

fn filter_json(j: &mut json::Json) {
    let mut replace = None;

    match *j {
        json::Object(ref mut ob) => {
            // remove
            ob.remove(&String::from_str("span"));
            ob.remove(&String::from_str("id"));
            let mut kv : Option<(String,Vec<Json>)> = None;
            match ob.get(&String::from_str("node")) {
                Some(&json::Object(ref ob2)) => {
                    match (ob2.get(&String::from_str("variant")),
                           ob2.get(&String::from_str("fields"))) {
                        (Some(&String(ref s)), Some(&List(ref ls))) => {
                            kv = Some((s.clone(), ls.clone()));
                        }
                        _ => ()
                    }
                }
                _ => ()
            }
            match kv {
                None => (),
                Some((k, v)) => {
                    ob.remove(&String::from_str("node"));
                    ob.insert(k, List(v));
                }
            }
            for (_, v) in ob.iter_mut() {
                filter_json(v);
            }
        }
        json::List(ref mut ls) => {
            for v in ls.iter_mut() {
                filter_json(v)
            }
            if ls.len() == 1 {
                replace = Some(ls.as_slice()[0].clone())
            }
        }
        _ => ()
    }
    match replace {
        None => (),
        Some(jj) => *j = jj
    }
}

static INDENT_STEP: int = 4;

fn print_indent(indent: int) {
    let mut out = io::stdout();
    for i in range(0, indent) {
        if i % INDENT_STEP == 0 {
            out.write_str("|");
        } else {
            out.write_str(" ");
        }
    }
}

fn print_sexp(indent: int, j: &json::Json) {
    let mut out = io::stdout();

    match *j {

        json::Object(ref ob) => {
            {
                for (k, v) in ob.iter() {
                    print_indent(indent);
                    out.write_str("(");
                    out.write_str(k.as_slice());
                    out.write_str("\n");
                    print_sexp(indent + INDENT_STEP, v);
                    print_indent(indent);
                    out.write_str(")\n");
                }
            }
        }
        json::List(ref ls) => {
            print_indent(indent);
            out.write_str("(\n");
            for v in ls.iter() {
                print_sexp(indent + INDENT_STEP, v);
            }
            print_indent(indent);
            out.write_str(")\n");
        }
        json::String(ref s) => {
            print_indent(indent);
            out.write_str(s.as_slice());
            out.write_str("\n");
        }
        json::Null => {
            print_indent(indent);
            out.write_str("<nil>\n");
        }
        json::Boolean(true) => {
            print_indent(indent);
            out.write_str("true\n");
        }
        json::Boolean(false) => {
            print_indent(indent);
            out.write_str("false\n");
        }
        json::I64(n) => {
            print_indent(indent);
            out.write_str(n.to_string().as_slice());
            out.write_str("\n");
        }
        json::U64(n) => {
            print_indent(indent);
            out.write_str(n.to_string().as_slice());
            out.write_str("\n");
        }
        json::F64(n) => {
            print_indent(indent);
            out.write_str(n.to_string().as_slice());
            out.write_str("\n");
        }
    }
}

fn main() {

    let args = os::args().into_iter().collect::<Vec<String>>();
    let opts = [ optflag("j", "", "dump output in JSON, not sexp") ];
    let matches = match getopts(args.tail(), opts) {
        Ok(m) => { m }
        Err(f) => { panic!(f) }
    };
    let dump_json = matches.opt_present("j");

    match io::stdin().read_to_string() {
        Ok(text) => {
            let opt = config::basic_options();
            let sess = session::build_session(opt, None,
                                              registry::Registry::new(rustc::DIAGNOSTICS));
            let cfg = config::build_configuration(&sess);
            let input = driver::StrInput(text);
            let cr = driver::phase_1_parse_input(&sess, cfg, &input);

            // JSON-ify, meaning "encode then re-parse as just json", ugh.
            let ast_str = json::encode(&cr.module);
            let chars = ast_str.as_slice().chars().collect::<Vec<char>>();
            let mut b = json::Builder::new(chars.into_iter());
            let mut j = b.build().ok().unwrap();

            filter_json(&mut j);
            if dump_json {
                let mut writer = io::stdout();
                j.to_pretty_writer(&mut writer as &mut io::Writer);
            } else {
                print_sexp(0, &j);
            }
        },
        Err(ioerr) => {
            panic!("I/O Error: {}", ioerr.desc);
        }
    }
}
