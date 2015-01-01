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

outfile = open('lexer.bad', 'w')

def chk(*args, **kwargs):
    output = ""
    try:
        output = subprocess.check_output(*args, **kwargs)
    except CalledProcessError:
        pass
    return output

def compare(p):
    if chk(flex, stdin=open(p)) != chk(rlex, stdin=open(p)):
        outfile.write(p + '\n')

for base, dirs, files in os.walk(sys.argv[3]):
    for f in filter(lambda p: p.endswith('.rs'), files):
        p = os.path.join(base, f)
        # compile-fail programs should be ignored
        # also, the lexer doesn't work with multibyte characters so
        # ignore programs that contain them
        if "compile-fail" in p or not all(ord(c) < 128 for c in open(p).read()):
            print("skipping {}".format(p))
            continue
        print("comparing {}".format(p))
        compare(p)

outfile.close()
