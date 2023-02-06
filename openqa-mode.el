;;; openqa-mode.el --- Create and refresh OpenQA buffers  -*- lexical-binding:t -*-

;;(require 'format-spec)
;;(require 'help-mode)
;;(require 'transient)
(require 'magit-section)
(require 'oqa)

(defvar bookmark-make-record-function)
(defgroup openqa nil
  "Inspect and manipulate Git repositories."
  :link '(info-link "OpenQA Buffer"))

;; (defmacro opeqa-setup-buffer (mode &optional locked &rest bindings)
;;   (declare (indent 2))
;;   `(openqa-setup-buffer-internal
;;     ,mode ,locked
;;     ,(cons 'list (mapcar (pcase-lambda (`(,var ,form))
;; 			   `(list ',var ,form))
;; 			 bindings))))

(defmacro with-openqa-buffer (&rest body)
  (declare (indent 0))
  `(let ((buffer (get-buffer-create "*openqa*")))
     (with-current-buffer buffer
       (let ((inhibit-read-only t))
	 (erase-buffer)
	 (magit-section-mode)
	 (magit-insert-section (openqa-buffer)
	   ,@body)))
     (switch-to-buffer-other-window buffer)))

;;; Mode

(defvar openqa-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-mode-map)
    (define-key map "j" #'openqa-jump)
    (define-key map [remap dired-jump] #'magit-dired-jump)
    map)
  "Keymap for `openqa-mode'.")

(transient-define-prefix openqa-jump ()
  "In a Magit-Status buffer, jump to a section."
  ["Jump to"
   [("s " "Status" magit-jump-to-stashes)]
     ;;:if (lambda () (memq 'magit-insert-stashes magit-status-sections-hook)))]
   [("i" "Using Imenu" imenu)]])

(define-derived-mode openqa-mode openqa-mode "OpenQA"
  "Mode for looking at OpenQA instance.
\\{openqa-mode-map}"
  :group 'openqa
  :lighter " openqa"
  :keymap
  (list (cons (openqa--key "t") #'whitespace-mode)
	(cons (openqa--key "u") #'whitespace-mode))

;;;###autoload
(defun magit-status-setup-buffer (&optional directory)
  ())

(defun openqa-display-tw-buf ()
  "Display Tumbleweed Test results"
  (with-openqa-buffer
    (magit-insert-section (group-overview)
      (magit-insert-heading
	(format "Tumbleweed - Test Results (%s)\n" default-directory))
      (magit-insert-section (group-overview)
	(insert (format " something %s " "foooo"))
	(insert (format " \n\n"))))
    
    (magit-insert-section (test-suites)
      (magit-insert-heading
	(format "Tumbleweed - Test Suites\n"))
      (magit-insert-section (test-suites)
	(insert (format " something %s " "bar"))))))


(openqa-display-tw-buf)
;; (defun openqa-display-buffer (buffer &optional display-function)
;;   "Display BUFFER in some window and maybe select it.
;; If optional DISPLAY-FUNCTION is non-nil, then use that to display
;; the buffer.  Otherwise use `magit-display-buffer-function', which
;; is the normal case.
;; Then, unless `magit-display-buffer-noselect' is non-nil, select
;; the window which was used to display the buffer.
;; Also run the hooks `magit-pre-display-buffer-hook'
;; and `magit-post-display-buffer-hook'."
;;   (with-current-buffer buffer
;;     (run-hooks 'magit-pre-display-buffer-hook))
;;   (let ((window (funcall (or display-function magit-display-buffer-function)
;; 			 buffer)))
;;     (unless magit-display-buffer-noselect
;;       (let* ((old-frame (selected-frame))
;; 	     (new-frame (window-frame window)))
;; 	(select-window window)
;; 	(unless (eq old-frame new-frame)
;; 	  (select-frame-set-input-focus new-frame)))))
;;   (with-current-buffer buffer
;;     (run-hooks 'magit-post-display-buffer-hook)))

;;(with-current-buffer "*scratch*"
;;    (message "Current buffer: %s" (current-buffer)))
;;(current-buffer))

(provide 'openqa-mode)
;;; magit-mode.el ends here
