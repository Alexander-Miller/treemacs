;;; treemacs.el --- A tree style file viewer package -*- lexical-binding: t -*-

;; Copyright (C) 2017 Alexander Miller

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;; File event watch and reaction implementation.

;;; Code:

(require 'dash)
(require 's)
(require 'f)
(require 'filenotify)
(require 'cl-lib)
(require 'treemacs-customization)
(require 'treemacs-impl)
(require 'treemacs-tags)

(declare-function treemacs-refresh "treemacs")

(defvar treemacs--collected-file-events nil
  "List of file change events treemacs needs to process.
If this is non-nil a timer to execute `treemacs--process-file-events' is
currently running.")

(defvar treemacs--collapsed-filewatch-hash (make-hash-table :size 100 :test #'equal)
  "Keeps track of dirs under filewatch due to being collapsed into one.

Collapsed directories require special handling since all directories of a series
need to be put under watch so as to be notified when the collapsed structure
needs to change, but removing the file watch is not straight forward:

Assume a series of directories are collapsed into one as '/c1/c2/c3/c4' and a
new file is created in '/c1/c2'. A refresh is started and only '/c1/c2' is
collapsed now, c3 and c4 are no longer part of the treemacs view and must be
removed from the filewatch list. However the event that triggered the refresh
was one of a file being created, so it is not possible to know that c3 and c4
need to stop being watched unless one also knows that they and c2 are under file
watch because they have been collapsed.

This is why this hash is used to keep track of collapsed directories under file
watch.")

(defvar treemacs--file-event-watchers nil
  "Alist of the descriptors of all currently active file event watch processes.
Car is the watched directory, cdr is the descriptor.")

(defvar treemacs--refresh-timer nil
  "Timer that will run a refresh after `treemacs-file-event-delay' ms.
Stored here to allow it to be cancelled by a manual refresh.")

(defvar treemacs--missed-refresh nil
  "Set to t when a file event causes a refresh and no treemacs window is shown.
This way treemacs knows to refresh itself the next time it becomes visible.")

(defsubst treemacs--refresh-catch-up ()
  "Do a refresh when it's has been missed."
  (when treemacs--missed-refresh
    (treemacs-refresh)
    (setq treemacs--missed-refresh nil)))

(defsubst treemacs--cancel-missed-refresh ()
  "Refresh missed refresh flag."
  (setq treemacs--missed-refresh nil))

(defsubst treemacs--cancel-refresh-timer ()
  "Cancel a the running refresh timer if it is active."
  (when treemacs--refresh-timer
    (cancel-timer treemacs--refresh-timer)
    (setq treemacs--refresh-timer nil)))

(defsubst treemacs--start-watching (path &optional collapse)
  "Watch PATH for file system events.
Also add PATH to `treemacs--collapsed-filewatch-hash' when COLLAPSE is non-nil."
  ;; no warning since the mode is defined in the same file
  (when (and (with-no-warnings treemacs-filewatch-mode)
             (not (assoc path treemacs--file-event-watchers)))
    (when collapse
      (puthash path t treemacs--collapsed-filewatch-hash))
    (push `(,path . ,(file-notify-add-watch path '(change) #'treemacs--filewatch-callback))
          treemacs--file-event-watchers)))

(defsubst treemacs--is-event-relevant? (event)
  "Decide if EVENT is relevant to treemacs or should be ignored.
An event counts as relevant when
1) The event's action is not \"stopped\".
2) The event's action is not \"changed\" while `treemacs-git-integration' is nil
3) The event's file will not return t when given to any of the functions which
   are part of `treemacs-ignored-file-predicates'."
  (let ((action (cl-second event))
        (dir    (cl-third event)))
    (not (or (equal action 'stopped)
             (and (equal action 'changed)
                  (not treemacs-git-integration))
             (--any? (funcall it (f-filename dir)) treemacs-ignored-file-predicates)))))

(defun treemacs--filewatch-callback (event)
  "Add EVENT to the list of file change events.
Start a timer to process the collected events if it has not been started
already. Do nothing if this event's file is irrelevant as per
`treemacs--is-event-relevant?'.
Also immediately remove the changed file from caches if it has been deleted
instead of waiting for file processing."
  (when (treemacs--is-event-relevant? event)
    (when (eq 'deleted (cadr event))
      (let ((path (cl-third event)))
        (treemacs--clear-from-cache path t)
        (treemacs--remove-all-tags-under-path-from-cache path)))
    (if treemacs--collected-file-events
        (push event treemacs--collected-file-events)
      (setq treemacs--collected-file-events (list event)
            treemacs--refresh-timer (run-at-time (format "%s millisecond" treemacs-file-event-delay)
                                                 nil #'treemacs--process-file-events)))))

(defsubst treemacs--stop-watching (path)
  "Stop watching PATH for file events.
This also means stopping the watch over all dirs below path."
  (setq treemacs--file-event-watchers
        (--reject (let ((watcher-path (car it)))
                    (when (or (equal watcher-path path)
                              (treemacs--is-path-in-dir? watcher-path path))
                      (file-notify-rm-watch (cdr it))
                      (remhash watcher-path treemacs--collapsed-filewatch-hash)
                      t))
                  treemacs--file-event-watchers)))

(defun treemacs--process-file-events ()
  "Process the file events that have been collected."
  (setq treemacs--refresh-timer nil)
  (while treemacs--collected-file-events
    (let* ((event   (pop treemacs--collected-file-events))
           (action  (cl-second event))
           (path    (cl-third event))
           (dir     (if (f-dir? path) path (f-dirname path))))
      (cond ((eq 'deleted action)
             (treemacs--stop-watching path))
            ((gethash dir treemacs--collapsed-filewatch-hash)
             (treemacs--stop-watching dir)))))
  (if (treemacs--is-visible?)
      (treemacs-refresh)
    (setq treemacs--missed-refresh t)))

(defun treemacs--stop-watching-all ()
  "Cancel any and all running file watch processes."
  (while treemacs--file-event-watchers
    (file-notify-rm-watch (cdr (pop treemacs--file-event-watchers))))
  (clrhash treemacs--collapsed-filewatch-hash)
  (setq treemacs--collected-file-events nil
        treemacs--missed-refresh nil))

(defsubst treemacs--tear-down-filewatch-mode ()
  "Stop watch processes, throw away file events, stop the timer."
  (treemacs--stop-watching-all)
  (treemacs--cancel-refresh-timer))

(define-minor-mode treemacs-filewatch-mode
  "Minor mode to let treemacs autorefresh itself on file system changes.
Activating this mode enables treemacs to watch the files it is displaying for
changes and automatically refresh itself by means of `treemacs-refresh' when it
detects a change that it decides is relevant.

A file event is relevant for treemacs if a new file has been created or deleted
or a file has been changed and `treemacs-git-integration' is t. Events caused
by files that are ignored as per `treemacs-ignored-file-predicates' are likewise
counted as not relevant.

The refresh is not called immediately after an event was received, treemacs
instead waits `treemacs-file-event-delay'ms to see if any more files have
changed to avoid having to refresh multiple times over a short period of time.
If the treemacs buffer exists, but is not visible, a refresh will be run the
next time it is shown.

The change only applies to directories opened *after* this mode has been
activated. This means that to enable file watching in an already existing
treemacs buffer it needs to be torn down and rebuilt by calling `treemacs' or
`treemacs-projectile'.

Turning off this mode is, on the other hand, instantaneous - it will immediately
turn off all existing file watch processes and outstanding refresh actions."
  :init-value nil
  :global     t
  :lighter    nil
  (unless treemacs-filewatch-mode
    (treemacs--tear-down-filewatch-mode)))

(provide 'treemacs-filewatch-mode)

;;; treemacs-filewatch-mode.el ends here
