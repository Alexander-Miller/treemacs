;;; treemacs.el --- A tree style file viewer package -*- lexical-binding: t -*-

;; Copyright (C) 2019 Alexander Miller

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
;;; Open directories are put under watch and file changes event collected even if filewatch-mode
;;; is disabled. This allows to remove deleted files from all the caches they are in. Activating
;;; filewatch-mode will therefore only enable automatic refresh of treemacs buffers.

;;; Code:

(require 'dash)
(require 's)
(require 'ht)
(require 'filenotify)
(require 'cl-lib)
(require 'treemacs-core-utils)
(require 'treemacs-async)
(require 'treemacs-dom)
(require 'treemacs-tags)
(require 'treemacs-macros)
(eval-and-compile
  (require 'inline))

(defvar treemacs--collapsed-filewatch-index (make-hash-table :size 100 :test #'equal)
  "Keeps track of dirs under filewatch due to being collapsed into one.

Collapsed directories require special handling since all directories of a series
need to be put under watch so as to be notified when the collapsed structure
needs to change, but removing the file watch is not straightforward:

Assume a series of directories are collapsed into one as '/c1/c2/c3/c4' and a
new file is created in '/c1/c2'. A refresh is started and only '/c1/c2' is
collapsed now, c3 and c4 are no longer part of the treemacs view and must be
removed from the filewatch list. However the event that triggered the refresh
was one of a file being created, so it is not possible to know that c3 and c4
need to stop being watched unless one also knows that they and c2 are under file
watch because they have been collapsed.

This is why this hash is used to keep track of collapsed directories under file
watch.")

(defvar treemacs--filewatch-index (make-hash-table :size 100 :test 'equal)
  "Hash of all directories being watched for changes.
A file path is the key, the value is a cons, its car is a list of the treemacs
buffers watching that path, its cdr is the watch descriptor.")

(defvar treemacs--refresh-timer nil
  "Timer that will run a refresh after `treemacs-file-event-delay' ms.
Stored here to allow it to be cancelled by a manual refresh.")

(define-inline treemacs--cancel-refresh-timer ()
  "Cancel a the running refresh timer if it is active."
  (inline-quote
   (when treemacs--refresh-timer
     (cancel-timer treemacs--refresh-timer)
     (setq treemacs--refresh-timer nil))))

(define-inline treemacs--start-watching (path &optional collapse)
  "Watch PATH for file system events.
Assumes to be run in the treemacs buffer as it will set PATH to be watched by
`current-buffer'.
Also add PATH to `treemacs--collapsed-filewatch-index' when COLLAPSE is non-nil.

PATH: Filepath
COLLAPSE: Bool"
  (inline-letevals (path collapse)
    (inline-quote
     (progn
       (when ,collapse
         (ht-set! treemacs--collapsed-filewatch-index ,path t))
       (-if-let (watch-info (ht-get treemacs--filewatch-index ,path))
           ;; just add current buffer to watch list if path is watched already
           (unless (memq (current-buffer) (car watch-info))
             (setcar watch-info (cons (current-buffer) (car watch-info))))
         ;; if the Tramp connection does not support watches, don't show an error
         ;; every time a watch is started.
         (treemacs-with-ignored-errors
          ((file-notify-error "No file notification program found"))
          ;; make new entry otherwise and set a new watcher
          (ht-set! treemacs--filewatch-index
                   ,path
                   (cons (list (current-buffer))
                         (file-notify-add-watch ,path '(change) #'treemacs--filewatch-callback)))))))))

(define-inline treemacs--stop-watching (path &optional all)
  "Stop watching PATH for file events.
This also means stopping the watch over all dirs below path.
Must be called inside the treemacs buffer since it will remove `current-buffer'
from PATH's watch list. Does not apply if this is called in reaction to a file
being deleted. In this case ALL is t and all buffers watching PATH will be
removed from the filewatch hashes.

PATH: Filepath
ALL: Bool"
  (inline-letevals (path all)
    (inline-quote
     (let (to-remove)
       (treemacs--maphash treemacs--filewatch-index (watched-path watch-info)
         (when (treemacs-is-path watched-path :in ,path)
           (let ((watching-buffers (car watch-info))
                 (watch-descr (cdr watch-info)))
             (if ,all
                 (progn
                   (file-notify-rm-watch watch-descr)
                   (ht-remove! treemacs--collapsed-filewatch-index watched-path)
                   (push watched-path to-remove))
               (when (memq (current-buffer) watching-buffers)
                 (if (cdr watching-buffers)
                     (setcar watch-info (delq (current-buffer) watching-buffers))
                   (file-notify-rm-watch watch-descr)
                   (ht-remove! treemacs--collapsed-filewatch-index watched-path)
                   (push watched-path to-remove)))))))
       (dolist (it to-remove)
         (ht-remove! treemacs--filewatch-index it))))))

(define-inline treemacs--is-event-relevant? (event)
  "Decide if EVENT is relevant to treemacs or should be ignored.
An event counts as relevant when
1) The event's action is not \"stopped\".
2) The event's action is not \"changed\" while `treemacs-git-mode' is disabled
3) The event's file will not return t when given to any of the functions which
   are part of `treemacs-ignored-file-predicates'."
  (declare (side-effect-free t))
  (inline-letevals (event)
    (inline-quote
     (let* ((action   (cl-second ,event))
            (dir      (cl-third ,event))
            (filename (treemacs--filename dir)))
       (not (or (eq action 'stopped)
                (and (eq action 'changed)
                     (not treemacs-git-mode))
                (--any? (funcall it filename dir) treemacs-ignored-file-predicates)))))))

(define-inline treemacs--set-refresh-flags (location type path)
  "Set refresh flags at LOCATION for TYPE and PATH in the dom of every buffer.
Also start the refresh timer if it's not started already."
  (inline-letevals (location type path)
    (inline-quote
     (when (with-no-warnings treemacs-filewatch-mode)
       (when (ht-get treemacs--collapsed-filewatch-index ,path)
         (ht-remove! treemacs--collapsed-filewatch-index ,path)
         (treemacs--stop-watching ,path))
       (treemacs-run-in-every-buffer
        (--when-let (treemacs-find-in-dom ,location)
          (let ((current-flag (assoc ,path (treemacs-dom-node->refresh-flag it))))
            (pcase (cdr current-flag)
              (`nil
               (push (cons ,path ,type) (treemacs-dom-node->refresh-flag it)))
              ('created
               (when (eq ,type 'deleted)
                 (setf (cdr current-flag) 'deleted)))
              ('deleted
               (when (eq ,type 'created)
                 (setf (cdr current-flag) 'created)))
              ('changed
               (when (eq ,type 'deleted)
                 (setf (cdr current-flag) 'deleted))))))
        (unless treemacs--refresh-timer
          (setf treemacs--refresh-timer
                (run-with-timer (/ treemacs-file-event-delay 1000) nil
                                #'treemacs--process-file-events))))))))

(defun treemacs--filewatch-callback (event)
  "Add EVENT to the list of file change events.
Do nothing if this event's file is irrelevant as per
`treemacs--is-event-relevant?'. Otherwise start a timer to process the collected
events if it has not been started already. Also immediately remove the changed
file from caches if it has been deleted instead of waiting for file processing."
  (when (treemacs--is-event-relevant? event)
    (-let [(_ event-type path) event]
      (when (eq 'deleted event-type)
        (treemacs--on-file-deletion path :no-buffer-delete))
      (if (eq 'renamed event-type)
          (let ((old-name path)
                (new-name (cl-fourth event)))
            (treemacs-run-in-every-buffer
             (treemacs--on-rename old-name new-name (with-no-warnings treemacs-filewatch-mode)))
            (treemacs--set-refresh-flags (treemacs--parent old-name) 'deleted old-name)
            (when (--none? (funcall it (treemacs--filename new-name) new-name) treemacs-ignored-file-predicates)
              (treemacs--set-refresh-flags (treemacs--parent new-name) 'created new-name)))
        (treemacs--set-refresh-flags (treemacs--parent path) event-type path)))))

(define-inline treemacs--do-process-file-events ()
  "Dumb helper function.
Extracted only so `treemacs--process-file-events' can decide when to call
`save-excursion' without code duplication."
  (inline-quote
   (treemacs-run-in-every-buffer
    (treemacs-save-position
     (-let [treemacs--no-messages (or treemacs-silent-refresh treemacs-silent-filewatch)]
       (treemacs--recursive-refresh)))
    (hl-line-highlight))))

(defun treemacs--process-file-events ()
  "Process the file events that have been collected.
Stop watching deleted dirs and refresh all the buffers that need updating."
  (setf treemacs--refresh-timer nil)
  (treemacs-without-following
   (if (eq treemacs--in-this-buffer t)
       (treemacs--do-process-file-events)
     ;; need to save excursion here because an update when the treemacs window is not visible
     ;; will actually move point in the current buffer
     ;; TODO(2019/07/18): check if this is still necessary after granular filewatch is done
     (save-excursion
       (treemacs--do-process-file-events)))))

(defun treemacs--stop-filewatch-for-current-buffer ()
  "Called when a treemacs buffer is torn down/killed.
Will stop file watch on every path watched by this buffer."
  (let ((buffer (treemacs-get-local-buffer))
        (to-remove))
    (treemacs--maphash treemacs--filewatch-index (watched-path watch-info)
      (-let [(watching-buffers . watch-descr) watch-info]
        (when (memq buffer watching-buffers)
          (if (= 1 (length watching-buffers))
              (progn
                (file-notify-rm-watch watch-descr)
                (ht-remove! treemacs--collapsed-filewatch-index watched-path)
                (push watched-path to-remove))
            (setcar watch-info (delq buffer watching-buffers))))))
    (dolist (it to-remove)
      (ht-remove! treemacs--filewatch-index it))))

(defun treemacs--stop-watching-all ()
  "Cancel any and all running file watch processes.
Clear the filewatch and collapsed filewatch indices.
Reset the refresh flags of every buffer.

Called when filewatch mode is disabled."
  (treemacs-run-in-every-buffer
   (treemacs--maphash treemacs-dom (_ node)
     (setf (treemacs-dom-node->refresh-flag node) nil)))
  (treemacs--maphash treemacs--filewatch-index (_ watch-info)
    (file-notify-rm-watch (cdr watch-info)))
  (ht-clear! treemacs--filewatch-index)
  (ht-clear! treemacs--collapsed-filewatch-index))

(define-inline treemacs--tear-down-filewatch-mode ()
  "Stop watch processes, throw away file events, stop the timer."
  (inline-quote
   (progn
     (treemacs--stop-watching-all)
     (treemacs--cancel-refresh-timer))))

(define-minor-mode treemacs-filewatch-mode
  "Minor mode to let treemacs autorefresh itself on file system changes.
Activating this mode enables treemacs to watch the files it is displaying (and
only those) for changes and automatically refresh its view when it detects a
change that it decides is relevant.

A file change event is relevant for treemacs if a new file has been created or
deleted or a file has been changed and `treemacs-git-mode' is enabled. Events
caused by files that are ignored as per `treemacs-ignored-file-predicates' are
counted as not relevant.

The refresh is not called immediately after an event was received, treemacs
instead waits `treemacs-file-event-delay' ms to see if any more files have
changed to avoid having to refresh multiple times over a short period of time.

The watch mechanism only applies to directories opened *after* this mode has
been activated. This means that to enable file watching in an already existing
treemacs buffer it needs to be torn down and rebuilt by calling `treemacs' or
`treemacs-projectile'.

Turning off this mode is, on the other hand, instantaneous - it will immediately
turn off all existing file watch processes and outstanding refresh actions."
  :init-value nil
  :global     t
  :lighter    nil
  (unless treemacs-filewatch-mode
    (treemacs--tear-down-filewatch-mode)))

;; in case we don't have a file notification library (like on travis CI)
(unless file-notify--library
  (fset 'treemacs--start-watching (lambda (_x &optional _y) (ignore)))
  (fset 'treemacs--stop-watching (lambda (_x &optional _y) (ignore))))

(treemacs-only-during-init (treemacs-filewatch-mode))

(provide 'treemacs-filewatch-mode)

;;; treemacs-filewatch-mode.el ends here
