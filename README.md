# Rust Lexer and Parser

This project is a lexer-parser combination capable of parsing Rust
code, circa 0.10ish. The purpose is to create a testable LL(1) grammar
specification for rust issue #2234. It contains a lexer specification
for flex and a grammar specification for
[LLnextgen](http://os.ghalkes.nl/LLnextgen/), and together they work
to gether to create a parser for Rust code.

## Lexer

The lexer is specified in `rust_lexer.l`. The rules contained are
primarily based off of how the rustc lexer works (defined in
src/libsyntax/parse/lexer.rs). It creates a lexer function that reads
from stdin and returns an int when it parses a token. Single-character
tokens like '+' return the ordinal number for that character. All
other tokens return a Token value defined in `rust_tokens.h`. The
lexer returns 0 on EOF, and -1 if it encounters an error.

### Notes

The lexer has a few notable differences from the real rust lexer:

* No unicode support
* No doc comment support. Doc comments are parsed the same as regular
  comments.
* No raw string literals. The syntax for raw strings cannot be parsed
  by regular expressions.

## Parser

The grammar for the parser is specified in `rust_parser.g`. The
`%token`s defined at the top must match the list in `rust_tokens.h` in
order for the lexer and parser to work together. The grammar is
organized to match the layout in the [Rust Reference
Manual](http://static.rust-lang.org/doc/master/rust.html). The rules
are primarily based off of the rustc parser's rules, so the names
generally correspond with with `parse_*` functions in rustc's
`parser.rs`. Note that the grammar intentionally omits obsolete
syntax.

## Building

This comes with a Makefile, so assuming you have both flex and
LLnextgen installed, you should be able to run `make` to build the
lexer and parser. A standalone lexer is produced as the `rust_lexer`
executable, and the parser is `rust_parser`.

## Further work

This parser is far from complete. Here are some major features still
missing:
* Ambiguities. They are noted in the code. Some may be resolved from
  refactoring or completing the grammar.
* Unicode support
* Tests, for both the lexer and parser

## Credits

Authored by Brian Leibig. Many parts of the grammar are based off of
John Clements's [rust-antlr
grammar](https://github.com/jbclements/rust-antlr).
