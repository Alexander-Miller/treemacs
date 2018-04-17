from os import listdir
from os.path import abspath, isdir, isfile
from posixpath import join
import sys
import os

ROOT     = sys.argv[1]
LIMIT    = int(sys.argv[2])
SHOW_ALL = sys.argv[3] == 't'

if LIMIT <= 0:
    exit(0)

def dir_content(path):
    ret = []
    for item in listdir(path):
        full_path = join(path, item)
        if os.access(full_path, os.R_OK) and (SHOW_ALL or item[0] != '.'):
            ret.append(full_path)
    return ret

def main():
    out  = sys.stdout
    dirs = [d for d in dir_content(ROOT) if isdir(d)]
    out.write("(")
    for current_dir in dirs:
        content   = dir_content(current_dir)
        collapsed = current_dir
        steps     = []
        depth     = 0
        while True:
            if len(content) == 1 and isdir(content[0]):
                single_path = content[0]
                collapsed   = join(collapsed, single_path)
                content     = dir_content(collapsed)
                depth      += 1
                steps.append(single_path)
                if depth >= LIMIT:
                    break
            else:
                break
        if depth > 0:
            final_dir      = steps[-1]
            display_suffix = final_dir[len(current_dir):]
            out.write("(" + '"' + current_dir + '" ' + '"' + display_suffix + '" ' + '"' + '" "'.join(steps) + '")')
    out.write(")")

main()
