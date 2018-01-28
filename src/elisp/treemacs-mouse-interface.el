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
;;; Functions relating to using the mouse in treemacs.

;;; Code:

;; noerror to be removed when emacs26 is released
(require 'xref nil t)
(require 'treemacs-impl)
(require 'treemacs-tags)
(eval-and-compile (require 'treemacs-macros))

;;;###autoload
(defun treemacs-node-buffer-and-position (&optional _)
  "Return source buffer or list of buffer and position for the current node.
This information can be used for future display. Stay in the selected window and
ignore any prefix argument."
  (interactive "P")
  (treemacs-without-messages
    (treemacs--execute-button-action
     :file-action (find-file-noselect (treemacs-safe-button-get btn :path))
     :dir-action (find-file-noselect (treemacs-safe-button-get btn :path))
     :tag-action (treemacs--tag-noselect btn)
     :window (selected-window)
     :save-window t
     :ensure-window-split nil
     :no-match-explanation "")))

(defun treemacs--imenu-tag-noselect (file tag-path)
  "Return a list of the source buffer for FILE and the position of the tag from TAG-PATH."
  (let ((tag (car tag-path))
        (path (cdr tag-path)))
    (condition-case e
        (progn
          (find-file-noselect file)
          (let ((index (treemacs--get-imenu-index file)))
            (dolist (path-item path)
              (setq index (cdr (assoc path-item index))))
            (-let [(buf . pos) (treemacs--pos-from-marker
                              (cdr (--first
                                    (equal (car it) tag)
                                    index)))]
              ;; some imenu implementations, like markdown, will only provide
              ;; a raw buffer position (an int) to move to
	      (list (or buf (get-file-buffer file)) pos))))
      (error
       (treemacs-log "Something went wrong when finding tag '%s': %s"
                      (propertize tag 'face 'treemacs-tags-face)
                      e)))))

(defun treemacs--tag-noselect (btn)
  "Return list of tag source buffer and position for BTN for future display."
  (cl-flet ((xref-definition
             (identifier)
             "Return the first definition of string IDENTIFIER."
             (car (xref-backend-definitions (xref-find-backend) identifier)))
            (xref-item-buffer
             (item)
             "Return the buffer in which xref ITEM is defined."
             (marker-buffer (save-excursion (xref-location-marker (xref-item-location item)))))
            (xref-item-position
             (item)
             "Return the buffer position where xref ITEM is defined."
             (marker-position (save-excursion (xref-location-marker (xref-item-location item))))))
    (-let [(tag-buf . tag-pos)
           (treemacs-with-button-buffer btn
             (-> btn (button-get :marker) (treemacs--pos-from-marker)))]
      (if tag-buf
          (list tag-buf tag-pos)
        (-pcase treemacs-goto-tag-strategy
          [`refetch-index
           (let (file tag-path)
             (with-current-buffer (marker-buffer btn)
               (setq file (treemacs--nearest-path btn)
                     tag-path (treemacs--tags-path-of btn)))
             (treemacs--imenu-tag-noselect file tag-path))]
          [`call-xref
           (let ((xref (xref-definition
                        (treemacs-with-button-buffer btn
                          (treemacs--get-label-of btn)))))
             (when xref
               (list (xref-item-buffer xref) (xref-item-position xref))))]
          [`issue-warning
           (treemacs-log "Tag '%s' is located in a buffer that does not exist."
                          (propertize (treemacs-with-button-buffer btn (treemacs--get-label-of btn)) 'face 'treemacs-tags-face))]
          [_ (error "[Treemacs] '%s' is an invalid value for treemacs-goto-tag-strategy" treemacs-goto-tag-strategy)])))))

(provide 'treemacs-mouse-interface)

;;; treemacs-mouse-interface.el ends here
