;;; treemacs.el --- A tree style file explorer package

;; Copyright (C) 2017 Alexander Miller

;; Author: Alexander Miller <alexanderm@web.de>
;; Package-Requires: ((emacs "24.4") (cl-lib "0.5") (dash "2.11.0") (s "1.10.0") (f "0.11.0") (ace-window "0.9.0") (pfuture "1.0"))
;; Homepage: https://github.com/Alexander-Miller/treemacs
;; Version: 1.6.5

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
;;; Main file only loads all features. Everything else is extracted into its
;;; own file to reduce clutter.

;;; Code:

(require 'treemacs-core)
(require 'treemacs-faces)
(require 'treemacs-customization)
(require 'treemacs-impl)
(require 'treemacs-mode)
(require 'treemacs-follow-mode)
(require 'treemacs-persist)
(require 'treemacs-filewatch-mode)

(provide 'treemacs)

;;; treemacs.el ends here
