;;; treemacs.el --- A tree style file viewer package

;; Copyright (C) 2017 Alexander Miller

;; Author: Alexander Miller <alexanderm@web.de>
;; Package-Requires: ((cl-lib "0.5") (dash "2.11.0") (s "1.10.0") (f "0.11.0") (ace-window "0.9.0"))
;; Homepage: https://github.com/Alexander-Miller/treemacs
;; Version: 1.0
;; Keywords: tree, file, explorer

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
;;; Follow mode definition only. Everything else is extracted into its
;;; own file to reduce clutter.

;;; Code:

(require 'f)
(require 's)

(defconst treemacs--persist-file (f-join user-emacs-directory ".cache" "treemacs-persist")
  "File treemacs uses to persist its current state.")

(defun treemacs--read-persist-data ()
  "Read the data stored in `treemacs--persist-file'."
    (when (f-file? treemacs--persist-file)
      (let ((ret   (list))
            (lines (s-lines (f-read treemacs--persist-file))))
        (while lines
          (let ((split (s-split " : " (pop lines))))
            (add-to-list 'ret
                         `(,(cl-first split) . ,(cl-second split)))))
        ret)))

(provide 'treemacs-persist)

;;; treemacs-persist.el ends here
