#![allow(unused_must_use)]

extern crate syntax;
extern crate rustc;
extern crate serialize;
extern crate getopts;

use getopts::{optflag,getopts};

use std::io;
use std::os;
use std::io::Reader;
use std::vec::FromVec;
use std::string::String;
use serialize::json;
use serialize::json::{Json, List, String, Object};

use rustc::driver::{driver, session, config};

fn filter_json(j: &mut json::Json) {
    let mut replace = None;

    match *j {
        json::Object(ref mut ob) => {
            // remove
            ob.pop(&String::from_str("lifetimes"));
            ob.pop(&String::from_str("global"));
            ob.pop(&String::from_str("types"));
            ob.pop(&String::from_str("span"));
            ob.pop(&String::from_str("id"));
            let mut kv : Option<(String,~[Json])> = None;
            match ob.find(&String::from_str("node")) {
                Some(&json::Object(ref ob2)) => {
                    match (ob2.find(&String::from_str("variant")),
                           ob2.find(&String::from_str("fields"))) {
                        (Some(&String(ref s)), Some(&List(ref ls))) => {
                            kv = Some((s.clone(), FromVec::from_vec(ls.clone())));
                        }
                        _ => ()
                    }
                }
                _ => ()
            }
            match kv {
                None => (),
                Some((k, v)) => {
                    ob.pop(&String::from_str("node"));
                    ob.insert(k, List(Vec::from_slice(v)));
                }
            }
            for (_, v) in ob.mut_iter() {
                filter_json(v);
            }
        }
        json::List(ref mut ls) => {
            for v in ls.mut_iter() {
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

static indent_step: int = 4;

fn print_indent(indent: int) {
    let mut out = io::stdout();
    for i in range(0, indent) {
        if i % indent_step == 0 {
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
                    print_sexp(indent + indent_step, v);
                    print_indent(indent);
                    out.write_str(")\n");
                }
            }
        }
        json::List(ref ls) => {
            print_indent(indent);
            out.write_str("(\n");
            for v in ls.iter() {
                print_sexp(indent + indent_step, v);
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
        json::Number(n) => {
            print_indent(indent);
            out.write_str(n.to_str().as_slice());
            out.write_str("\n");
        }
    }
}

fn main() {

    let args = os::args().move_iter().map(|s| String::from_owned_str(s)).collect::<Vec<String>>();
    let opts = [ optflag("j", "", "dump output in JSON, not sexp") ];
    let matches = match getopts(args.tail(), opts) {
        Ok(m) => { m }
        Err(f) => { fail!(f.to_err_msg()) }
    };
    let dump_json = matches.opt_present("j");

    match io::stdin().read_to_str() {
        Ok(text) => {
            let opt = config::basic_options();
            let sess = session::build_session(opt, None);
            let cfg = config::build_configuration(&sess);
            let input = driver::StrInput(String::from_owned_str(text));
            let cr = driver::phase_1_parse_input(&sess, cfg, &input);

            // JSON-ify, meaning "encode then re-parse as just json", ugh.
            let ast_str = json::Encoder::str_encode(&cr.module);
            let mut s = ast_str.to_str();
            let chars = Vec::from_fn(s.len(), |_| s.pop_char().unwrap());
            let mut b = json::Builder::new(chars.move_iter());
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
            fail!("I/O Error: {}", ioerr.desc);
        }
    }
}
