from subprocess import Popen, PIPE
from os import listdir
from os.path import isfile, isdir
from posixpath import join
import sys

GIT_ROOT  = str.encode(sys.argv[1])
GIT_CMD   = "git status --porcelain --ignored ."
STDOUT    = sys.stdout.buffer
OPEN      = b'("'
CLOSE     = b'")'
CONS      = b'" . "'
output    = b""

def print_all_untracked_files(path):
    global output
    for item in listdir(path):
        full_path = join(path, item)
        output += OPEN + b'?' + CONS + full_path + CLOSE
        if isdir(full_path):
            print_all_untracked_files(full_path)

def main():
    global output
    proc = Popen(GIT_CMD, shell=True, stdout=PIPE, bufsize=100)
    dirs = {}

    output += b'('
    for item in proc.stdout:
        if item.startswith(b' '):
            item = item[1:]
        state, filename = item.split(b' ', 1)

        # renames have the form STATE OLDNAME -> NEWNAME
        # final newline must be trimmed as well
        if state == b"R":
            full_root = join(GIT_ROOT, filename.split(b' -> ')[1][:-1])
        else:
            full_root = join(GIT_ROOT, filename.lstrip()[:-1])

        # filename is a directory, final slash must be removed
        if full_root.endswith(b'/'):
            full_root = full_root[:-1]
            output += OPEN + state + CONS + full_root + CLOSE
            dirs[full_root] = True
        else:
            output += OPEN + state + CONS + full_root + CLOSE
        # for files deeper down in the file hierarchy also print all their directories
        # if ./foo/bar/baz.el is changed then ./foo and ./foo/bar must be shown as changed as well
        if b'/' in filename and state != b"!!":
            name_parts = filename.split(b'/')[:-1]
            dirname = b''
            for name_part in name_parts:
                dirname = join(dirname, name_part)
                full_dirname = join(GIT_ROOT, dirname.lstrip())
                # directories should not be printed more than once, which would happen if
                # e.g. both ./foo/x and ./foo/y have changes
                if full_dirname not in dirs:
                    output += OPEN + b'M' + CONS + full_dirname + CLOSE
                    dirs[full_dirname] = True
        if state.startswith(b'?') and isdir(full_root):
            print_all_untracked_files(full_root)
    output += b')'
    STDOUT.write(output)

main()
