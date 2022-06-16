from subprocess import Popen, PIPE
import sys

STATUS_CMD = "git status -sb"

def main():
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
    behind = 0

    for inf in line[i_open+1 : i_close].split(b", "):
        split = inf.split(b" ")
        status = split[0]
        number = int(split[1])
        if status == b"ahead":
            ahead = number
        elif status == b"behind":
            behind = number

    if ahead == 0 and behind != 0:
        print('#(" ↓{}" 0 3 (face treemacs-git-commit-diff-face))'.format(behind))
    elif ahead != 0 and behind == 0:
        print('#(" ↑{}" 0 3 (face treemacs-git-commit-diff-face))'.format(ahead))
    else:
        print('#(" ↑{} ↓{}" 0 6 (face treemacs-git-commit-diff-face))'.format(ahead, behind))

main()
