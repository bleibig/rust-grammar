extern crate syntax;
extern crate rustc;
extern crate serialize;

use std::io;
use std::io::Reader;
use std::vec::FromVec;
use std::strbuf::StrBuf;
use serialize::json;
use serialize::json::{Json, List, String, Object};

use rustc::driver::{driver, session, config};

fn filter_json(j: &mut json::Json) {
    let mut replace = None;

    match *j {
        json::Object(ref mut ob) => {
            // remove
            ob.pop(&StrBuf::from_str("lifetimes"));
            ob.pop(&StrBuf::from_str("global"));
            ob.pop(&StrBuf::from_str("types"));
            ob.pop(&StrBuf::from_str("span"));
            ob.pop(&StrBuf::from_str("id"));
            let mut kv : Option<(StrBuf,~[Json])> = None;
            match ob.find(&StrBuf::from_str("node")) {
                Some(&json::Object(ref ob2)) => {
                    match (ob2.find(&StrBuf::from_str("variant")),
                           ob2.find(&StrBuf::from_str("fields"))) {
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
                    ob.pop(&StrBuf::from_str("node"));
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

fn main() {
    match io::stdin().read_to_str() {
        Ok(text) => {
            let opt = config::basic_options();
            let sess = session::build_session(opt, None);
            let cfg = config::build_configuration(&sess);
            let input = driver::StrInput(StrBuf::from_owned_str(text));
            let cr = driver::phase_1_parse_input(&sess, cfg, &input);

            // JSON-ify, meaning "encode then re-parse as just json", ugh.
            let ast_str = json::Encoder::str_encode(&cr.module);
            let s = ast_str.to_str();
            let mut b = json::Builder::new(s.chars());
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
