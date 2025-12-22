from subprocess import Popen, PIPE
import sys
from os import environ

GIT_BIN = sys.argv[1]
STATUS_CMD = "{} status -sb".format(GIT_BIN)

def main():
    environ["LC_ALL"] = "C"
    proc = Popen(STATUS_CMD, shell=True, stdout=PIPE, bufsize=100)

    if (proc.wait() != 0):
        sys.exit(2)

    line = proc.stdout.readline()
    i_open = line.find(b"[")
    i_close = line.find(b"]")

    if (i_open == -1):
        print("nil")
        sys.exit(0)

    ahead = 0
    ahead_len = 0
    behind = 0
    behind_len = 0

    for inf in line[i_open+1 : i_close].split(b", "):
        split = inf.split(b" ")
        status = split[0]
        text = split[1]
        number = int(text)
        if status == b"ahead":
            ahead = number
            ahead_len = len(text)
        elif status == b"behind":
            behind = number
            behind_len = len(text)

    if ahead == 0 and behind != 0:
        face_len = 2 + behind_len
        print('#(" ↓{}" 0 {} (face treemacs-git-commit-diff-face))'.format(behind, face_len))
    elif ahead != 0 and behind == 0:
        face_len = 2 + ahead_len
        print('#(" ↑{}" 0 {} (face treemacs-git-commit-diff-face))'.format(ahead, face_len))
    else:
        face_len = 4 + ahead_len + behind_len
        print('#(" ↑{} ↓{}" 0 {} (face treemacs-git-commit-diff-face))'.format(ahead, behind, face_len))

main()
