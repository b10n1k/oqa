;;; oqa.el --- OpenQA Framework  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Ioannis Bonatakis

;; Author: Ioannis Bonatakis <ybonatakis@suse.com>
;; Keywords: tools

;; Version: 0.0.0
;; Package-Requires: ((emacs "24.1"))
;; URL: https://github.com/b10n1k/oqa

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; OpenQA Framework

;;; Code:

(defgroup oqa ()
  "OpenQA Framework"
  :group 'tools
  :prefix "oqa-"
  :link '(url-link "https://github.com/b10n1k/oqa"))

(defun main ()
  "oqa starts here"
  ;;(interactive)
  ;; (debug)
  (message "hello")
  )

(define-key global-map (kbd "C-M-z") 'main)

;;(main)

(provide 'oqa)
;;; oqa.el ends here
