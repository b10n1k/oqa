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

(transient-define-prefix oqa-opensuse ()
  "Prefix that waves with overridden suffix behavior."
  [(tsc-suffix-wave-macroed
    :transient nil
    :key "stw"
    :description "TW status"
    :command oqa-status)])
(transient-define-prefix oqa-suse ()
  "Prefix that waves with overridden suffix behavior."
  [(tsc-suffix-wave-macroed
    :transient nil
    :key "shpc"
    :description "wave overridingly"
    :command tsc--wave-override)])

(defun get-nth-opensuse-build (n)
  (let* ((json-hash (with-current-buffer
		      (url-retrieve-synchronously (format "https://openqa.opensuse.org/group_overview/1.json"))
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

(defun oqa-status ()
  "Return the status of the Groupid <jid>
  ARGS is the arguments list from transient."
  ;;(interactive (list (transient-args 'oqa-transient)))
  (interactive)
  (switch-to-buffer "*oqa_results*")
  (setq tabulated-list-format [("Distri" 15) ("Total" 8) ("Passes" 8) ("Failed" 8) ("Unfinished" 10)])
  (setq tabulated-list-entries (list (get-nth-opensuse-build 0)))
  (tabulated-list-init-header)
  (tabulated-list-print))

;; suffix cmds
;; https://magit.vc/manual/transient/Defining-Suffix-and-Infix-Commands.html
;; (defun oqa-status (jid)
;;   "Return the status of the Groupid <jid>
;;   ARGS is the arguments list from transient."
;;   (interactive (list (transient-args 'oqa-transient)))

;;   (switch-to-buffer "*oqa_results*")
;;   (setq tabulated-list-format [("Distri" 15) ("Total" 8) ("Passes" 8) ("Failed" 8) ("Unfinished" 10)])
;;   (setq tabulated-list-entries (list (get-nth-opensuse-build 0 1)))
;;   (tabulated-list-init-header)
;;   (tabulated-list-print))

(defun tsc-suffix-wave ()
  "General Testing function."
  (interactive)
  (message "Waves at the user at: %s." (current-time-string)))
(defun tsc--wave-override ()
  "Vanilla command used to override suffix's commands."
  (interactive)
    (message "This suffix was overridden.  I am what remains."))
(transient-define-prefix tsc-wave-overridden ()
  "Prefix that waves with overridden suffix behavior."
  [(tsc-suffix-wave-macroed
    :transient nil
    :key "O"
    :description "wave overridingly"
    :command tsc--wave-override)])

;; popups
;; preffix cmds
(transient-define-prefix oqa-transient ()
  "oqa Menu"
  ["OpenQA Instance"

   ;; this suffix will not exit after calling sub-prefix
   ("ooo" "openqa.opensuse.org" tsc-wave-overridden)
   ("osd" "openqa.suse.de" tsc-wave :transient t)])
  ;; ["Job Groups" :description "group overview parent"
  ;; ;;["Actions":
  ;;  ("g" "parent groups" tsc-suffix-wave :description "List all groups")
  ;;  ;; list all groups with status
  ;;   ("d" tsc-suffix-wave :description "Dispay certain group")]
  ;;  [:description "Build"
  ;; 		 ("bl" "latest build" (lambda ()
  ;; 			     (interactive)
  ;; 			     (message "suffix wave too much called")))
  ;; 		 ("bs" "build settings" (lambda ()
  ;; 			     (interactive)
  ;; 			     (message "suffix wave excessively called")))]
  ;;  ("s" "OpenQA Group status" oqa-status)])

;;;###autoload
(defun oqa ()
  "Invoke the oqa buffer.
  DIRECTORY is optional for TRAMP support."
  (interactive)
  (oqa--save-line)
  (oqa--pop-to-buffer (oqa--buffer-name))
  (when directory (setq default-directory directory))
  (oqa-mode)
  ;; (message (concat "Namespace: " openqa-namespace))
  )

(defvar oqa-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "o") 'oqa-transient)
    map))

;; major mode oqa-mode
(define-derived-mode oqa-mode tabulated-list-mode "openqa"
  "Special mode for oqa buffers."
  (buffer-disable-undo)
  (kill-all-local-variables)
  (setq truncate-lines t)
  (setq mode-name "oqa-mode")
  (setq major-mode 'oqa-mode)
  (use-local-map oqa-mode-map)
  (setq tabulated-list-sort-key nil)
  (tabulated-list-init-header)
  (tabulated-list-print)
  (hl-line-mode 1)
  (oqa-status 1))

(provide 'oqa)
;;; oqa.el ends here
