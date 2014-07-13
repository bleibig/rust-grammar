#!/usr/bin/env python

import sys

import os
import subprocess

# ARGUMENTS:
# 1 - path to the parser executable
# 2 - path to the parser-lalr executable
# 3 - path to the source directory to look for *.rs files

if len(sys.argv) != 4:
    print 'usage: testparser.py <parser> <parser-lalr> <rust-src-dir>'
    sys.exit(1)

parser = sys.argv[1]
parser_lalr = sys.argv[2]

# flex dies on multibyte characters
BLACKLIST = ['libstd/str.rs', 'libstd/strbuf.rs', 'libstd/ascii.rs']

def chk(*args, **kwargs):
    return subprocess.check_output(*args, **kwargs)

def compare(p):
    if chk(flex, stdin=open(p)) != chk(rlex, stdin=open(p)):
        raise Exception("{} differed between the reference lexer and libsyntax's lexer".format(p))

total = 0
parser_ok = 0
parser_lalr_ok = 0

bad_parser = []
bad_parser_lalr = []
print "\n"

for base, dirs, files in os.walk(sys.argv[3]):
    for f in filter(lambda p: p.endswith('.rs'), files):
        p = os.path.join(base, f)
        if any([p.endswith(b) for b in BLACKLIST]):
            continue

        total += 1
        try:
            if len(chk(parser, stdin=open(p), stderr=subprocess.STDOUT)) == 0:
                parser_ok += 1
            else:
                bad_parser.append(p)
        except subprocess.CalledProcessError:
            bad_parser.append(p)
            pass
        try:
            if "syntax error" not in chk(parser_lalr, stdin=open(p), stderr=subprocess.STDOUT):
                parser_lalr_ok += 1
            else:
                bad_parser_lalr.append(p)
        except subprocess.CalledProcessError:
            bad_parser_lalr.append(p)
            pass

        sys.stdout.write("\033[2K\r total: %d, parser: %d, parser-lalr: %d, scanned %-60s" %
                        (total, parser_ok, parser_lalr_ok, p))

print "\n"

for (filename, bad, parser) in [("parser.bad", bad_parser, parser),
                                ("parser-lalr.bad", bad_parser_lalr, parser_lalr)]:
    print("writing %d files that failed to parse with %s to %s" % (len(bad), parser, filename))
    with open(filename, "w") as f:
          for p in bad:
              f.write(p)
              f.write("\n")
