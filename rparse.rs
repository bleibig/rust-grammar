extern crate syntax;
extern crate rustc;
extern crate serialize;

use rustc::driver::driver;
use rustc::driver::session;
use std::io;
use std::io::Reader;
use serialize::json;
use serialize::json::{Json, List, String, Object};

fn filter_json(j: &mut json::Json) {
    match *j {
        json::Object(ref mut ob) => {
            // remove
            ob.pop(&~"span");
            ob.pop(&~"id");
            let mut kv : Option<(~str,~[Json])> = None;
            match ob.find(&~"node") {
                Some(&json::Object(ref ob2)) => {
                    match (ob2.find(&~"variant"),
                           ob2.find(&~"fields")) {
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
                    ob.pop(&~"node");
                    ob.insert(k, List(v));
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
            let opt = session::basic_options();
            let sess = driver::build_session(opt, None);
            let cfg = driver::build_configuration(&sess);
            let input = driver::StrInput(text);
            let cr = driver::phase_1_parse_input(&sess, cfg, &input);

            // JSON-ify, meaning "encode then re-parse as just json", ugh.
            let ast_str = json::Encoder::str_encode(&cr);
            let mut p = json::Parser::new(ast_str.chars());
            let mut j = p.parse().unwrap();

            filter_json(&mut j);

            let mut writer = io::stdout();
            let _ = j.to_pretty_writer(&mut writer as &mut io::Writer);
        },
        Err(ioerr) => {
            fail!("I/O Error: {}", ioerr.desc);
        }
    }
}
