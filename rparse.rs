extern crate syntax;
extern crate rustc;
extern crate serialize;

use std::io;
use std::io::Reader;
use std::vec::FromVec;
use serialize::json;
use serialize::json::{Json, List, String, Object};

use rustc::driver::{driver, session, config};

fn filter_json(j: &mut json::Json) {
    match *j {
        json::Object(ref mut ob) => {
            // remove
            ob.pop(&"span".to_owned());
            ob.pop(&"id".to_owned());
            let mut kv : Option<(~str,~[Json])> = None;
            match ob.find(&"node".to_owned()) {
                Some(&json::Object(ref ob2)) => {
                    match (ob2.find(&"variant".to_owned()),
                           ob2.find(&"fields".to_owned())) {
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
                    ob.pop(&"node".to_owned());
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
        }
        _ => ()
    }
}

fn main() {
    match io::stdin().read_to_str() {
        Ok(text) => {
            let opt = config::basic_options();
            let sess = session::build_session(opt, None);
            let cfg = config::build_configuration(&sess);
            let input = driver::StrInput(text);
            let cr = driver::phase_1_parse_input(&sess, cfg, &input);

            // JSON-ify, meaning "encode then re-parse as just json", ugh.
            let ast_str = json::Encoder::str_encode(&cr);
            let mut b = json::Builder::new(ast_str.chars());
            let mut j = b.build().ok().unwrap();

            filter_json(&mut j);

            let mut writer = io::stdout();
            let _ = j.to_pretty_writer(&mut writer as &mut io::Writer);
        },
        Err(ioerr) => {
            fail!("I/O Error: {}", ioerr.desc);
        }
    }
}
