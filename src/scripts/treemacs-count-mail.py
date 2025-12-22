from subprocess import Popen, PIPE
import sys
from os import environ

# Command line arguments are a list of maildirs.
# The output is a list of items in the form '((P1 A1) (P2 A2))' where P is the node path for a maildir
# node, and A is the mail count annotation text
# Exmaple: '(((treemacs-mu4e "/web/") " (176)")((treemacs-mu4e "/web/" "/web/Inbox") " (161)"))'

UNREAD_CMD  = "mu find maildir:'{}'  --fields 'i' flag:'unread' 2> /dev/null | wc -l"
PATH_PREFIX = "treemacs-mu4e"
LOCAL_PREFIX = "/" + sys.argv[1]

def main():
    maildirs = sys.argv[2:]

    output = ["("]
    for maildir in maildirs:

        mu_dir = maildir
        is_local = False
        is_leaf = not maildir.endswith("/")
        # "Local Folders" is an artificial maildir that is used to group
        # otherwise free standing folders under a single header like
        # in thunderbird
        if mu_dir.startswith(LOCAL_PREFIX):
            is_local = True
            mu_dir = mu_dir.replace(LOCAL_PREFIX, "")
            if mu_dir == "/":
                continue

        environ["LC_ALL"] = "C"
        unread = Popen(UNREAD_CMD.format(mu_dir.replace(" ", "\ ")),
                       shell=True,
                       stdout=PIPE,
                       bufsize=100,
                       encoding='utf-8'
                    ).communicate()[0][:-1]

        if unread == "0":
            continue

        node_path = []
        path_item = "/"
        split_path = maildir.split("/")[1:] if is_leaf else maildir.split("/")[1:-1]

        # it makes difference for mu whether a maildir ends in a slash or not
        for i in range(0, len(split_path) - 1):
            path_item = path_item + split_path[i] + "/"
            node_path.append("\"" + path_item + "\"")
        final_item = "" if is_leaf else "/"
        node_path.append("\"" + path_item + split_path[-1] + final_item + "\"")

        suffix = '" ({})"'.format(unread)

        output.append('(({} {}) {})'.format(
            PATH_PREFIX, " ".join(node_path), suffix
        ))

    output.append(")")
    print("".join(output))

main()
