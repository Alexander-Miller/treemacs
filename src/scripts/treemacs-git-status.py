from subprocess import Popen, PIPE
from os import listdir, environ
from os.path import isdir, islink
from posixpath import join
import sys

# The script is supplied with the followig command line arguments
# 1) the repository root - used only for file-joining to an absolute path
#    the actual working directory is set in emacs
# 2) `treemacs-max-git-entries`
# 3) `treemacs-git-command-pipe`
# 4) directory face mode - either "modified" or a comma-separated severity
#    hierarchy like "U,M,?,A,R" (see `treemacs-git-directory-face-mode')
# 5) a list of expanded directories the script may recurse into to collect
#    an entry for every untracked/ignored file inside
#    this list is turned into a set since it is possible that it contains duplicates
#    when called for magit, see also `treemacs-magit--extended-git-mode-update`

GIT_BIN      = sys.argv[1]
GIT_ROOT     = str.encode(sys.argv[2])
LIMIT        = int(sys.argv[3])
GIT_CMD      = "{} status --porcelain --ignored=matching . ".format(GIT_BIN) + sys.argv[4]
DIR_MODE     = sys.argv[5] if len(sys.argv) > 5 else "modified"
STDOUT       = sys.stdout.buffer
RECURSE_DIRS = set([str.encode(it[(len(GIT_ROOT)):]) + b"/" for it in sys.argv[6:]]) if len(sys.argv) > 6 else []

if DIR_MODE == "modified":
    SEVERITY_ORDER = None
else:
    SEVERITY_ORDER = [s.encode() for s in DIR_MODE.split(",")]
QUOTE        = b'"'
output       = []
ht_size      = 0

def face_for_status(status):
    if status == b"M":
        return b"treemacs-git-modified-face"
    elif status == b"U":
        return b"treemacs-git-conflict-face"
    elif status == b"?":
        return b"treemacs-git-untracked-face"
    elif status == b"!":
        return b"treemacs-git-ignored-face"
    elif status == b"A":
        return b"treemacs-git-added-face"
    elif status == b"R":
        return b"treemacs-git-renamed-face"
    else:
        return b"font-lock-keyword-face"

def resolve_dir_face(statuses):
    """Resolve a directory's face from its children's statuses using the severity hierarchy."""
    for status in SEVERITY_ORDER:
        if status in statuses:
            return face_for_status(status)
    return b"treemacs-git-modified-face"

def find_recursive_entries(path, state):
    global output, ht_size
    for item in listdir(path):
        full_path = join(path, item)
        output.append(QUOTE + full_path.replace(b'"', b'\\"') + QUOTE + face_for_status(state))
        ht_size += 1
        if ht_size > LIMIT:
            break
        if isdir(full_path) and not islink(full_path):
            find_recursive_entries(full_path, state)

def main():
    global output, ht_size
    # Don't lock Git when updating status.
    environ["LC_ALL"] = "C"
    environ["GIT_OPTIONAL_LOCKS"] = "0"
    proc = Popen(GIT_CMD, shell=True, stdout=PIPE, bufsize=100)
    dirs_added = {}
    dirs_statuses = {} if SEVERITY_ORDER is not None else None

    for item in proc.stdout:
        # remove final newline
        item = item[:-1]

        # remove leading space if item was e.g. modified only in the worktree
        if item.startswith(b' '):
            item = item[1:]
        state, filename = item.split(b' ', 1)

        # reduce the state to a single-letter-string
        state = state[0:1]

        filename = filename.strip()
        # sometimes git outputs quoted filesnames
        if filename.startswith(b'"'):
            filename = filename[1:-1]

        # find the absolute path for the current item
        # renames have the form STATE OLDNAME -> NEWNAME
        abs_path = None
        if state == b"R":
            abs_path = join(GIT_ROOT, filename.split(b' -> ')[1])
        else:
            abs_path = join(GIT_ROOT, filename.lstrip())

        # filename is a directory, final slash must be removed
        if abs_path.endswith(b'/'):
            abs_path = abs_path[:-1]
            dirs_added[abs_path] = True
        output.append(QUOTE + abs_path + QUOTE + face_for_status(state))
        ht_size += 1

        # for files deeper down in the file hierarchy also print all their directories
        # if /A/B/C/x is changed then /A and /A/B and /A/B/C must be shown as changed as well
        if b'/' in filename and state != b'!':
            name_parts = filename.split(b'/')[:-1]
            dirname = b''
            for name_part in name_parts:
                dirname = join(dirname, name_part)
                full_dirname = join(GIT_ROOT, dirname.lstrip())
                # directories should not be printed more than once, which would happen if
                # e.g. both /A/B/C/x and /A/B/C/y have changes
                if SEVERITY_ORDER is None:
                    # modified mode: existing behavior
                    if full_dirname not in dirs_added:
                        output.append(QUOTE + full_dirname.replace(b'"', b'\\"') + QUOTE + b"treemacs-git-modified-face")
                        ht_size += 1
                        dirs_added[full_dirname] = True
                else:
                    # severity mode: collect statuses, defer output
                    if full_dirname not in dirs_statuses:
                        dirs_statuses[full_dirname] = set()
                    dirs_statuses[full_dirname].add(state)
                    dirs_added[full_dirname] = True
        # for untracked and ignored directories we need to find an entry for every single file
        # they contain
        # however this applies only for directories that are expanded and whose content is visible
        if state in [b'?', b'!'] and isdir(abs_path):
            if filename in RECURSE_DIRS:
                find_recursive_entries(abs_path, state)
        if ht_size >= LIMIT:
            break

    # second pass: emit deferred directory entries for severity mode
    if dirs_statuses is not None:
        for full_dirname, statuses in dirs_statuses.items():
            output.append(QUOTE + full_dirname.replace(b'"', b'\\"') + QUOTE + resolve_dir_face(statuses))
            ht_size += 1

    STDOUT.write(
        b"#s(hash-table size " + \
        bytes(str(ht_size), 'utf-8') + \
        b" test equal rehash-size 1.5 rehash-threshold 0.8125 data ("
    )
    if ht_size > 0:
      STDOUT.write(b"".join(output))
    STDOUT.write(b"))")

    sys.exit(proc.poll())

main()
