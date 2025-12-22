from subprocess import Popen, PIPE
from os.path import exists
from os import environ
import sys

GIT_BIN = sys.argv[1]
GIT_CMD = "{} clean -ndX".format(GIT_BIN)
STDOUT  = sys.stdout.buffer

def quote(string):
    return b'"' + string + b'"'

def process_git_output(root, proc):
    root_bytes = bytes(root, "utf-8")

    count = 0
    for line in proc.stdout:
        # output has the form 'Would remove /a/b/c'
        # final newline and final slash also need to go
        path = line.replace(b"Would remove ", b"")[:-1]
        if path.endswith(b"/"):
            path = path[:-1]

        ignored_file = root_bytes + b"/" + path
        ignored_file_parent = ignored_file[:ignored_file.rindex(b"/")]

        STDOUT.write(quote(ignored_file_parent))
        STDOUT.write(quote(ignored_file))

        # arbitrary limit of no more than 100 files
        count += 1
        if count > 100:
            break

def main():
    roots  = sys.argv[2:]
    procs  = []

    for root in roots:
        if exists(root + "/.git"):
            environ["LC_ALL"] = "C"
            proc = Popen(GIT_CMD, shell=True, stdout=PIPE, bufsize=100, cwd=root)
            procs.append((root, proc))

    STDOUT.write(b"(")

    for (root, proc) in procs:
        process_git_output(root, proc)

    STDOUT.write(b")")

main()
