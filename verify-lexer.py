#!/usr/bin/env python

# ARGUMENTS:
# 1 - path to the flex-lexer executable
# 2 - path to the rlex executable
# 3 - path to the source directory to look for *.rs files

import sys

import os
import subprocess

flex = sys.argv[1]
rlex = sys.argv[2]

# flex dies on multibyte characters
BLACKLIST = ['libstd/str.rs', 'libstd/strbuf.rs', 'libstd/ascii.rs']

def chk(*args, **kwargs):
    return subprocess.check_output(*args, **kwargs)

def compare(p):
    if chk(flex, stdin=open(p)) != chk(rlex, stdin=open(p)):
        raise Exception("{} differed between the reference lexer and libsyntax's lexer".format(p))

for base, dirs, files in os.walk(sys.argv[3]):
    for f in filter(lambda p: p.endswith('.rs'), files):
        p = os.path.join(base, f)
        die = False
        for black in BLACKLIST:
            if p.endswith(black):
                print("skipping {}".format(p))
                die = True
                break

        if die:
            continue

        print("comparing {}".format(p))
        compare(p)
