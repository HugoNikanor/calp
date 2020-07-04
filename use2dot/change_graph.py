#!/usr/bin/env python3

import re
import colorsys
import hashlib
import sys

def md5(str):
    return hashlib.md5(str.encode("UTF-8")).hexdigest()

def rgb(str):
    return md5(str)[0:6]

def main(args):

    if len(args) < 3:
        print("Usage: ./change_graph.py <infile> <outfile>")
        return

    [_, infile, outfile, *rest] = args

    with open(infile) as f:
        lines = f.readlines()
        # [3:-1]

    f = open(outfile, 'w')

    for line in lines:
        m = re.search('^( *"\(([^)]*)\)" -> "(\([^)]*\))");', line)
        if m:
            f.write(f'{m.group(1)} [color="#{rgb(m.group(2))}"];\n')
        else:
            f.write(line)
        # colorsys.hsv_to_rgb(


#  "(server macro)" -> "(ice-9 regex)";

if __name__ == "__main__":
    main(sys.argv)
