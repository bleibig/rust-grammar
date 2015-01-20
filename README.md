# Rust Lexer and Parser

This project is a lexer-parser combination capable of parsing Rust
code for Rust 1.0 (currently in alpha). The purpose is to create a
testable LALR grammar specification for rust issue #2234. It contains
a lexer specification for flex and a grammar specification for [GNU
Bison](https://www.gnu.org/software/bison/), and together they work
together to create a parser for Rust code. The parser should be able
to accept all programs accepted by `rustc -Z parse-only`.

## Lexer

The lexer is specified in `lexer.l`. The rules contained are primarily
based off of how the rustc lexer works (defined in
src/libsyntax/parse/lexer.rs). It creates a lexer function that reads
from stdin and returns an int when it parses a token. Single-character
tokens like '+' return the ordinal number for that character. All
other tokens return a Token value defined in `tokens.h`. The lexer
returns 0 on EOF, and -1 if it encounters an error.

## Parser

The grammar for the parser is specified in `parser-lalr.y`. The
grammar specification is divided into five parts:

1. Items and attributes (top level stuff)
2. Patterns
3. Types
4. Blocks, statements, and expressions
5. Macros and misc. rules

Like the standalone lexer, it reads from stdin and outputs to
stdout. In addition to being a recognizer for Rust, if "-v" is passed
in as a command line argument, the parser from this grammar also
builds and prints an AST in an s-expression format.

## Building

A makefile is provided and building is handled by running
`make`. Building requires flex 2.5.35 or later, and bison 3.0.2 or
later to both be installed.

On OS X, the Xcode toolchain provides an older version of bison
(2.3). This will not work with the grammar in this project, so you
will have to download and install version 3.0.2 or later.

Building of rlex and rparse do not (yet) support cargo, use make or
just invoke directly with rustc.

## Testing

Two scripts are provided for testing the parser or just the lexer.

* verify-lexer.py

Should be invoked like `./verify-lexer.py ./lexer ./rlex /path/to/rust/source/files`

It will run both lexers on all *.rs files and compare the output of
./lexer to ./rlex. If the lexing output is different, the file will be
listed in lexer.bad at the end of the run.

* testparser.py 

Should be invoked like `./testparser.py -p ./parser-lalr -s /path/to/rust/source/files

You can have it test multiple rust parsers with multiple args after
the -p option.

It will run the parser on all *.rs files in the directory
specified. Files that fail to parse are signified by the parser
returning nonzero exit status, and all files that fail to parse will
be listed in parser-lalr.bad.

Note that both tools are designed around testing the official rust
sources, but should work with any directories containing valid rust
code. They are hard-coded to ignore files in the "compile-fail"
directory.

## Other tools

* rlex: This tool reads rust code from stdin and uses rustc's lexer to
  output tokens to stdout, one per line. This can be used to verify
  the flex lexer works the same as rustc's lexer.

* rparse: This tool reads rust code from stdin and uses rustc's parser
  to print the AST to stdout in either s-expression or JSON format.

## Other files

Brief rundown of the other files in this project:

* `lexer_main.c`: Contains the main function for the standalone lexer

* `parser-lalr-main.c`: Contains the main function and code for

* creating and managing S-expressions, used by the grammar file.

* `tokens.c`: Contains the `print_token` function used by
  `lexer_main.c` to generate its output.

* `tokens.h`: Defines a big enum for all the rust tokens wider than a
  single character.

## Caveats

* The s-expression output from parser-lalr is not a complete or
  accurate representation of the AST that rustc creates when it
  parses, rather, it's just an approximation, so it's not meant to be
  diffed with rparse's output.

* Unicode is supported poorly, the lexer just happily accepts bytes
  where the msb is 1 wherever unicode is accepted, which means it can
  accept invalid UTF-8 sequences.

* A goal for this project is not to support obsolete syntax.  Given
  the many syntax changes to rust over the past months, it's possible
  that support for some obsolete syntax may still be lingering around.
