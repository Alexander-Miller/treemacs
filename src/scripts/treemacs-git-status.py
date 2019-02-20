from subprocess import Popen, PIPE
from os import listdir
from os.path import isfile, isdir
from posixpath import join
import sys

GIT_ROOT  = str.encode(sys.argv[1])
LIMIT     = int(sys.argv[2])
GIT_CMD   = "git status --porcelain --ignored . " + sys.argv[3]
STDOUT    = sys.stdout.buffer
QUOTE      = b'"'
output    = b""
ht_size   = 0

def print_all_untracked_files(path):
    global output, ht_size
    for item in listdir(path):
        full_path = join(path, item)
        output += QUOTE + full_path + QUOTE + QUOTE + b'?' + QUOTE
        ht_size += 1
        if isdir(full_path):
            print_all_untracked_files(full_path)

def main():
    global output, ht_size
    proc = Popen(GIT_CMD, shell=True, stdout=PIPE, bufsize=100)
    dirs = {}
    iter_count = 0

    for item in proc.stdout:
        if item.startswith(b' '):
            item = item[1:]
        state, filename = item.split(b' ', 1)

        # sometimes git outputs quoted filesnames
        if filename.startswith(b'"'):
            filename = filename[1:-1]

        # renames have the form STATE OLDNAME -> NEWNAME
        # final newline must be trimmed as well
        if state == b"R":
            full_root = join(GIT_ROOT, filename.split(b' -> ')[1][:-1])
        else:
            full_root = join(GIT_ROOT, filename.lstrip()[:-1])

        # filename is a directory, final slash must be removed
        if full_root.endswith(b'/'):
            full_root = full_root[:-1]
            output += QUOTE + full_root + QUOTE + QUOTE + state[0:1] + QUOTE
            dirs[full_root] = True
        else:
            output += QUOTE + full_root + QUOTE + QUOTE + state[0:1] + QUOTE
        ht_size += 1
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
                    output += QUOTE + full_dirname + QUOTE + QUOTE + b'M' + QUOTE
                    ht_size += 1
                    dirs[full_dirname] = True
        if state.startswith(b'?') and isdir(full_root):
            print_all_untracked_files(full_root)
        iter_count += 1
        if iter_count >= LIMIT:
            break
    elisp_ht = b"#s(hash-table size " + \
        bytes(str(ht_size), 'utf-8') + \
        b" test equal rehash-size 1.5 rehash-threshold 0.8125 data (" + \
        output + \
        b"))"
    STDOUT.write(elisp_ht)


    sys.exit(proc.poll())

main()
