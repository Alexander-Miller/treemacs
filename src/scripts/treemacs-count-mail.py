from subprocess import Popen, PIPE
import sys

# mu4e-headers-include-related
REL_FLAG    = "-r" if sys.argv[1] == "True" else ""
UNREAD_CMD  = "mu find maildir:'{}' " + REL_FLAG + " --fields 'i' flag:'unread' 2> /dev/null | wc -l"
PATH_PREFIX = "treemacs-mu4e"

def main():
    maildirs = sys.argv[2:]

    ret = ["("]
    for maildir in maildirs:

        unread = Popen(UNREAD_CMD.format(maildir), shell=True, stdout=PIPE, bufsize=100, encoding='utf-8').communicate()[0][:-1]

        if unread == "0":
            continue

        path = []
        path_item = ""
        split_path = maildir.split("/")[1:]

        # the script must have access to the true folder for the count to work
        # when passing things back to elisp the pseudo-hierarchy must be re-established
        if len(split_path) == 1:
            split_path.insert(0, "Local Folders")

        for split_part in split_path:
            path_item = path_item + "/" + split_part
            path.append("\"" + path_item + "\"")

        suffix = '" ({})"'.format(unread) if unread != "0" else ""

        ret.append('(({} {}) {} {})'.format(
            PATH_PREFIX, " ".join(path), suffix, "treemacs-mu4e-mailcount-face" )
        )

    ret.append(")")
    print("".join(ret))

main()
