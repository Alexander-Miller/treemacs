from subprocess import run, Popen, PIPE, DEVNULL, check_output
import sys
import os
from os import environ

# There are 3+ command line arguments:
# 1) the file to update
# 2) the file's previous state, to check if things changed at all
# 3) the file's parents that need to be updated as well

GIT_BIN = sys.argv[1]
FILE = sys.argv[2]
OLD_FACE = sys.argv[3]
PARENTS = [p for p in sys.argv[4:]]

FILE_STATE_CMD = "{} status --porcelain --ignored=matching ".format(GIT_BIN)
IS_IGNORED_CMD = "{} check-ignore ".format(GIT_BIN)
IS_TRACKED_CMD = "{} ls-files --error-unmatch ".format(GIT_BIN)
IS_CHANGED_CMD = "{} ls-files --modified --others --exclude-standard ".format(GIT_BIN)

def face_for_status(path, status):
    if status == "M":
        return "treemacs-git-modified-face"
    elif status == "U":
        return "treemacs-git-conflict-face"
    elif status == "?":
        return "treemacs-git-untracked-face"
    elif status == "!":
        return "treemacs-git-ignored-face"
    elif status == "A":
        return "treemacs-git-added-face"
    elif status == "R":
        return "treemacs-git-renamed-face"
    elif os.path.isdir(path):
        return "treemacs-directory-face"
    else:
        return "treemacs-git-unmodified-face"

def main():
    if '"' in FILE or '\\' in FILE:
        sys.exit(2)

    new_state = determine_file_git_state()

    # nothing to do
    if OLD_FACE == face_for_status(FILE, new_state):
        sys.exit(2)

    proc_list = []

    # for every parent file start all necessary git processes immediately
    # even if we don't need them later
    for p in PARENTS:
        add_git_processes(proc_list, p)

    result_list = [(FILE, new_state)]

    # iterate through the parents and propagate ignored and untracked states downwards
    # the following states are possible for *directories*:
    # 0 -> clean
    # ! -> ignored
    # ? -> untracked
    # M -> modified
    i = 0
    l = len(proc_list)
    propagate_state = None
    while i < l:
        path, ignore_proc, tracked_proc, changed_proc = proc_list[i]
        if ignore_proc.communicate() and ignore_proc.returncode == 0:
            propagate_state = "!"
            result_list.append((path, propagate_state))
            break
        elif tracked_proc.communicate() and tracked_proc.returncode == 1:
            propagate_state = "?"
            result_list.append((path, propagate_state))
            break
        elif changed_proc.communicate()[0] != b'' and changed_proc.returncode == 0:
            result_list.append((path, "M"))
        else:
            result_list.append((path, "0"))
        i += 1

    if propagate_state:
        i += 1
        while i < l:
            result_list.append((proc_list[i][0], propagate_state))
            i += 1

    elisp_conses = "".join(['("{}" . {})'.format(path, face_for_status(path, state)) for path, state in result_list])
    elisp_alist = "({})".format(elisp_conses)
    print(elisp_alist)

def add_git_processes(status_listings, path):
    environ["LC_ALL"] = "C"
    ignored_proc = Popen(IS_IGNORED_CMD + path, shell=True, stdout=DEVNULL, stderr=DEVNULL)
    tracked_proc = Popen(IS_TRACKED_CMD + path, shell=True, stdout=DEVNULL, stderr=DEVNULL)
    changed_proc = Popen(IS_CHANGED_CMD + path, shell=True, stdout=PIPE,    stderr=DEVNULL)

    status_listings.append((path, ignored_proc, tracked_proc, changed_proc))

def determine_file_git_state():
    environ["LC_ALL"] = "C"
    proc = Popen(FILE_STATE_CMD + FILE, shell=True, stdout=PIPE, stderr=DEVNULL)
    line = proc.stdout.readline()
    if line:
        state = line.lstrip().split(b" ")[0]
        return state.decode('utf-8').strip()[0]
    else:
        return "0"

main()
