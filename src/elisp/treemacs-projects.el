;;; treemacs.el --- A tree style file viewer package -*- lexical-binding: t -*-

;; Copyright (C) 2018 Alexander Miller

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
;;; TODO

;;; Code:

(require 'dash)
(require 'treemacs-impl)
(eval-and-compile
  (require 'cl-lib))

(defvar-local treemacs--projects nil)

(defsubst treemacs-project->name (project)
  "Get the name of PROJECT."
  (aref project 1))

(defsubst treemacs-project->path (project)
  "Get the path of PROJECT."
  (aref project 2))

(defsubst treemacs-project->position (project)
  "Get the order number of PROJECT."
  (aref project 3))

(defsubst treemacs-project->order (project)
  "Get the order number of PROJECT."
  (aref project 4))

(defsubst treemacs-project->is-expanded? (project)
  "Return non-nil if PROJECT is expanded."
  (eq 'root-node-open (button-get (treemacs-project->position project) :state)))

(with-no-warnings
  (cl-defstruct (treemacs-project (:conc-name treemacs-project->))
    name path position order))

(defsubst treemacs-project-at-point ()
  "Get the `cl-struct-treemacs-project' for the (nearest) project at point."
  (-let [btn (treemacs-current-button)]
    ;; if we're not on a button now look for the next best or previous best button
    (unless btn
      (--if-let (next-button (point))
          (setq btn it)
        (--if-let (previous-button (point))
            (setq btn it)
          (error "[Treemacs] It looks like the treemacs buffer is empty. That shouldn't happen!"))))
    (-let [project (button-get btn :project)]
      (while (not project)
        (setq btn (button-get btn :parent)
              project (button-get btn :project)))
      project)))

(provide 'treemacs-projects)

;;; treemacs-projects.el ends here
