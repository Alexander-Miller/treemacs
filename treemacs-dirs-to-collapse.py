from os import listdir
from os.path import abspath, join, isdir, isfile
import sys

def dir_content(path):
    return [join(path, item) for item in listdir(path)]

ROOT  = sys.argv[1]
LIMIT = int(sys.argv[2])
dirs  = [d for d in dir_content(ROOT) if isdir(d)]
out   = sys.stdout

if LIMIT <= 0:
    exit(0)

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
