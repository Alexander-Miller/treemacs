from os import listdir
from os.path import isdir
from posixpath import join
import sys
import os

ROOT     = sys.argv[1]
LIMIT    = int(sys.argv[2])
SHOW_ALL = sys.argv[3] == 't'

# special workaround for windows platforms
# the default `join' implementation cannot quite deal with windows
# paths in the form of "C:/A/B" & "C:/A/B/C", joining them as
# "C:/A/B/C:/A/B/C"
# it can, however, be "tricked" into doing the right thing by adding
# a slash to the start of the paths
# go figure
if sys.platform == 'win32':
    def join_dirs(d1, d2, full_path=False):
        missing_slash = False
        if not d1.startswith("/"):
            missing_slash = True
            d1 = "/" + d1
        # full_path is only True when the second argument is
        # another absolute path
        if full_path and not d2.startswith("/"):
            missing_slash = True
            d2 = "/" + d2
        joined = join(d1, d2)
        if missing_slash:
            # still need to return the joined path without the
            # leading slash, the way it looked originally
            return joined[1:]
        else:
            return joined
else:
    def join_dirs(d1, d2, *_):
        return join(d1, d2)

if LIMIT <= 0:
    exit(0)

def dir_content(path):
    """
    returns the content of given path, excluding unreadable files
    and dotfiles (unless SHOW_ALL is True)
    """
    ret = []
    for item in listdir(path):
        full_path = join_dirs(path, item)
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
                collapsed   = join_dirs(collapsed, single_path, True)
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
            out.write("(" + '"' + display_suffix + '" ' + '"' + current_dir + '" ' + '"' + '" "'.join(steps) + '")')
    out.write(")")

main()
