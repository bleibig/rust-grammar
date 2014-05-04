extern crate syntax;
extern crate rustc;
extern crate serialize;

use rustc::driver::driver;
use rustc::driver::session;
use std::io;
use std::io::Reader;
use serialize::{json, Encodable};

fn main() {
    match io::stdin().read_to_str() {
	Ok(text) => {
	    let opt = session::basic_options();
	    let sess = driver::build_session(opt, None);
	    let cfg = driver::build_configuration(&sess);
	    let input = driver::StrInput(text);
	    let cr = driver::phase_1_parse_input(&sess, cfg, &input);

	    let mut writer = io::stdout();
	    let mut enc = json::PrettyEncoder::new(&mut writer as &mut io::Writer);
	    let _ = cr.encode(&mut enc);
	},
	Err(ioerr) => {
	    fail!("I/O Error: {}", ioerr.desc);
	}
    }
}
