#!/usr/bin/env python

# Transforms a lidr file into a Pandoc markdown file

from sys import argv

def emit_code_lines(f, lines):
    f.write("```idris\n")
    for line in lines:
        f.write(line)
    f.write("```\n")

def main():
    outpath = argv[1]
    inpath = argv[2]
    code_lines = []
    with open(inpath, 'rt') as f_in:
        with open(outpath, 'wt') as f_out:
            for line in f_in.readlines():
                if line[0] == '>':
                    code_lines.append(line[2:])
                else:
                    if code_lines:
                        emit_code_lines(f_out, code_lines)
                        code_lines = []
                    f_out.write(line)
                

if __name__ == '__main__':
    main()
