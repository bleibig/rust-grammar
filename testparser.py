#!/usr/bin/env python

import sys

import os
import subprocess
import argparse

# usage: testparser.py [-h] [-p PARSER [PARSER ...]] -s SOURCE_DIR

# Parsers should read from stdin and return exit status 0 for a
# successful parse, and nonzero for an unsuccessful parse

parser = argparse.ArgumentParser()
parser.add_argument('-p', '--parser', nargs='+')
parser.add_argument('-s', '--source-dir', nargs=1, required=True)
args = parser.parse_args(sys.argv[1:])

total = 0
ok = {}
bad = {}
for parser in args.parser:
    ok[parser] = 0
    bad[parser] = []
devnull = open(os.devnull, 'w')
print "\n"

for base, dirs, files in os.walk(args.source_dir[0]):
    for f in filter(lambda p: p.endswith('.rs'), files):
        p = os.path.join(base, f)
        compile_fail = 'compile-fail' in p
        ignore = any('ignore-test' in line or 'ignore-lexer-test' in line
                     for line in open(p).readlines())
        if compile_fail or ignore:
            continue
        total += 1
        for parser in args.parser:
            if subprocess.call(parser, stdin=open(p), stderr=subprocess.STDOUT, stdout=devnull) == 0:
                ok[parser] += 1
            else:
                bad[parser].append(p)
        parser_stats = ', '.join(['{}: {}'.format(parser, ok[parser]) for parser in args.parser])
        sys.stdout.write("\033[2K\r total: %d, %s, scanned %-60s" %
                         (total, parser_stats, p))

devnull.close()

print "\n"

for parser in args.parser:
    filename = os.path.basename(parser) + '.bad'
    print("writing %d files that failed to parse with %s to %s" % (len(bad[parser]), parser, filename))
    with open(filename, "w") as f:
          for p in bad[parser]:
              f.write(p)
              f.write("\n")
