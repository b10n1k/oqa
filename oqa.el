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

(require 'transient)
(require 'dash)
(require 's)
;;(require 'yaml-mode)
(require 'tramp)
(require 'subr-x)
(require 'eshell)
(require 'dired)

(defgroup oqa ()
  "OpenQA Framework"
  :group 'tools
  :prefix "oqa-"
  :link '(url-link "https://github.com/b10n1k/oqa"))

(defun main ()
  "oqa starts here"
  (interactive)
  ;; (debug)
  ;;(message "hello")
  (oqa)
  )

(define-key global-map (kbd "C-M-z") 'main)

(defun get-nth-opensuse-build (n)
  (let* ((json-hash (with-current-buffer
			(url-retrieve-synchronously "https://openqa.opensuse.org/group_overview/1.json")
		      (goto-char (point-min))
		      (re-search-forward "^$")
		      (delete-region (point) (point-min))
		      (json-parse-buffer)))
	 (builds (gethash "build_results" json-hash))
	 (last-build (aref builds n)))
    (message "%S" last-build)
    (list nil
	  (vector
	   (gethash "version" last-build)
	   (number-to-string (gethash "total" last-build))
	   (number-to-string (gethash "passed" last-build))
	   (number-to-string (gethash "failed" last-build))
	   (number-to-string (gethash "unfinished" last-build))))))

(defun oqa-status (&optional jid)
  "Return the status of the Groupid <jid>

ARGS is the arguments list from transient."
  (interactive (list (transient-args 'oqa-transient)))

  (switch-to-buffer "*oqa_results*")
  (setq tabulated-list-format [("Distri" 15) ("Total" 8) ("Passes" 8) ("Failed" 8) ("Unfinished" 10)])
  (setq tabulated-list-entries (list (get-nth-opensuse-build 0)))
  (tabulated-list-init-header)
  (tabulated-list-print))
;; (defun oqa-status (&optional jid)
;;   "Return the status of the Groupid <jid>

;; ARGS is the arguments list from transient."
;;     (let* ((json-hash (with-current-buffer
;;                           (url-retrieve-synchronously "https://openqa.opensuse.org/group_overview/1.json")
;;                         (goto-char (point-min))
;;                         (re-search-forward "^$")
;;                         (delete-region (point) (point-min))
;;                         (json-parse-buffer)))
;;            (builds (gethash "build_results" json-hash))
;;            (last-build (aref builds n)))
;;       (message "%S" last-build)
;;       (list nil
;;             (vector
;;              (gethash "version" last-build)
;;              (number-to-string (gethash "total" last-build))
;;              (number-to-string (gethash "passed" last-build))
;;              (number-to-string (gethash "failed" last-build))
;;              (number-to-string (gethash "unfinished" last-build)))))
;;   (interactive (list (transient-args 'oqa-log-popup)))
;;   (switch-to-buffer "*oqa_results*")
;;   (setq tabulated-list-format columns)
;;   (setq tabulated-list-entries rows)
;;   (tabulated-list-init-header)
;;   (tabulated-list-print))

;; popups

(transient-define-prefix oqa-transient ()
  "oqa Menu"
  ["Actions"
   ("s" "OpenQA Group status" oqa-status)])

;;;###autoload
(defun oqa ()
  "Invoke the oqa buffer.
DIRECTORY is optional for TRAMP support."
  (interactive)
  ;;(oqa--save-line)
  ;;(oqa--pop-to-buffer (oqa--buffer-name))
  ;;(when directory (setq default-directory directory))
  (oqa-mode)
  ;; (message (concat "Namespace: " openqa-namespace))
  )

(defvar oqa-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "o") 'oqa-transient)
    map))

(define-derived-mode oqa-mode tabulated-list-mode "openqa"
  "Special mode for oqa buffers."
  (buffer-disable-undo)
  (kill-all-local-variables)
  (setq truncate-lines t)
  (setq mode-name "oqa")
  (setq major-mode 'oqa-mode)
  ;;(use-local-map oqa-mode-map)
  (setq tabulated-list-sort-key nil)
  (tabulated-list-init-header)
  (tabulated-list-print)
  (hl-line-mode 1))

(provide 'oqa)
;;; oqa.el ends here
