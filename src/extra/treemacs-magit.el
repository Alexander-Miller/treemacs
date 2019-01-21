;;; treemacs-magit.el --- Magit integration for treemacs -*- lexical-binding: t -*-

;; Copyright (C) 2018 Alexander Miller

;; Author: Alexander Miller <alexanderm@web.de>
;; Package-Requires: ((treemacs "0.0") (magit "2.90.0"))
;; Package-Version: 0
;; Homepage: https://github.com/Alexander-Miller/treemacs

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
;;; Closing the gaps for filewatch- and git-modes in conjunction with magit.
;;; Specifically this package will hook into magit so as to artificially
;;; produce filewatch events for changes that treemacs would otherwise
;;; not catch, nameley the committing and (un)staging of files.

;;; Code:

(require 'treemacs)
(require 'magit)

;; no need for dash for a single when-let
(eval-when-compile
  (when (version< emacs-version "26")
    (defalias 'if-let* #'if-let)
    (defalias 'when-let* #'when-let)))

(defvar treemacs--magit-timers nil
  "Cached list of roots an update is scheduled for.")

(defun treemacs--magit-schedule-update ()
  "Schedule an update to potentially run after 2 seconds of idle time.
In order for the update to fully run several conditions must be met:
 * A timer for an update for the given dir must not already exist
   (see `treemacs--magit-timers')
 * The dir must be part of a treemacs workspace, and
 * The project must not be set for refresh already."
  (let ((magit-root (magit-toplevel)))
    (unless (member magit-root treemacs--magit-timers)
      (push magit-root treemacs--magit-timers)
      (run-with-idle-timer
       3 nil
       (lambda ()
         (setf treemacs--magit-timers (delete magit-root treemacs--magit-timers))
         (treemacs-run-in-every-buffer
          (when-let* ((project (treemacs--find-project-for-path magit-root)))
            (let* ((project-root (treemacs-project->path project))
                   (dom-node (treemacs-get-from-shadow-index project-root)))
              (unless (treemacs-shadow-node->refresh-flag dom-node)
                (treemacs--set-refresh-flags project-root))))))))))

(unless (featurep 'treemacs-magit)
  (add-hook 'magit-post-commit-hook #'treemacs--magit-schedule-update)
  (add-hook 'git-commit-post-finish-hook #'treemacs--magit-schedule-update)
  (add-hook 'magit-post-stage-hook #'treemacs--magit-schedule-update)
  (add-hook 'magit-post-unstage-hook #'treemacs--magit-schedule-update))

(provide 'treemacs-magit)

;;; treemacs-magit.el ends here
