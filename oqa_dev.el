(require 'transient)
(require 'dash)
(require 's)
;;(require 'yaml-mode)
(require 'tramp)
(require 'subr-x)
(require 'eshell)
(require 'dired)

;; https://github.com/positron-solutions/transient-showcase
(transient-define-prefix tsc-hello ()
			 "Prefix that is minimal and uses an anonymous command suffix."
			 [("s" "call suffix"
			   (lambda ()
			     (interactive)
			     (message "Called a suffix")))
			  ("x" "call suffix"
			   (lambda ()
			     (interactive)
			     (message "Called a suffix")))])

;; First, use M-x org-babel-execute-src-blk to cause `tsc-hello' to be defined
;; Second, M-x `eval-last-sexp' with your point at the end of the line below
(tsc-hello)

(transient-define-prefix tsc-wave-keyword-args ()
  "Prefix that waves at the user persistently."
  [("e" "wave eventually & stay" tsc--wave-eventually :transient t)
   ("s" "wave surely & leave" tsc--wave-surely :transient nil)])

(tsc-wave-keyword-args)

(transient-define-prefix tsc-wave ()
  "Prefix that waves at the user"
    [("w" "wave" tsc-suffix-wave)])
;; tsc-suffix-wave-suffix defined above

(transient-define-prefix tsc-wave-macro-defined ()
  "Prefix to wave using a macro-defined suffix."
  [(tsc-suffix-wave-macroed)]) ; note, information moved from prefix to the suffix.

;; (tsc-wave-macro-defined)

(defun tsc-suffix-wave ()
  "Wave at the user."
  (interactive)
  (message "Waves at the user at: %s." (current-time-string)))

(transient-define-prefix tsc-layout-descriptions ()
  "Prefix with descriptions specified with slots."
  ["Let's Give This Transient a Title\n" ; yes the newline works
   ["O3"
    ("g" "groups" (lambda ()
			     (interactive)
			     (message "suffix wave once called")))
    ("wa" "wave again" (lambda ()
			     (interactive)
			     (message "suffix wave again called")))]

   ["OSD"
    ("g" "groups" (lambda ()
			     (interactive)
			     (message "suffix wave some called")))
    ("wb" "wave better" (lambda ()
			     (interactive)
			     (message "suffix wave better called")))]]

  ["Bad title" :description "Group of Groups"
   ["Group Three"
    ("k" "bad desc" tsc-suffix-wave :description "key-value wins" :transient nil)
    ("n" tsc-suffix-wave :description "no desc necessary")]
   [:description "Key Only Def"
		 ("wt" "wave too much" (lambda ()
			     (interactive)
			     (message "suffix wave too much called")))
		 ("we" "wave excessively" (lambda ()
			     (interactive)
			     (message "suffix wave excessively called")))]])

(tsc-layout-descriptions)

(transient-define-suffix tsc-suffix-wave-macroed ()
  "Prefix that waves with macro-defined suffix."
  :transient t
  :key "T"
  :description "wave from macro definition"
  (interactive)
  (message "Waves from a macro definition at: %s" (current-time-string)))

;; Suffix definition creates a command
;; (tsc-suffix-wave-macroed)
;; Because that's where the suffix object is stored
;; (get 'tsc-suffix-wave-macroed 'transient--suffix)
;; tsc-suffix-wave-suffix defined above

(transient-define-prefix tsc-wave-macro-defined ()
  "Prefix to wave using a macro-defined suffix."
  [(tsc-suffix-wave-macroed)]) ; note, information moved from prefix to the suffix.

(tsc-wave-macro-defined)

(transient-define-prefix tsc-layout-stacked ()
  "Prefix with layout that stacks groups on top of each other."
  ["Top Group" ("wt" "wave top" tsc-suffix-wave)]
  ["Bottom Group" ("wb" "wave bottom" tsc-suffix-wave)])

(tsc-layout-stacked)

(transient-define-prefix tsc-layout-columns ()
  "Prefix with side-by-side layout."
  [["Left Group" ("wl" "wave left" tsc-suffix-wave)]
   ["Right Group" ("wr" "wave right" tsc-suffix-wave)]])

(tsc-layout-columns)

(transient-define-prefix tsc-layout-stacked-columns ()
  "Prefix with stacked columns layout."
  ["Top Group"
   ("wt" "wave top" tsc-suffix-wave)]

  [["Left Group"
    ("wl" "wave left" tsc-suffix-wave)]
   ["Right Group"
    ("wr" "wave right" tsc-suffix-wave)]])

(tsc-layout-stacked-columns)

(transient-define-prefix tsc-layout-the-grid ()
  "Prefix with groups in a grid-like arrangement."

  [:description "The Grid\n" ; must use slot or macro is confused
		["Left Column" ; note, no newline
		 ("ltt" "left top top" tsc-suffix-wave)
		 ("ltb" "left top bottom" tsc-suffix-wave)
		 ""
		 ("lbt" "left bottom top" tsc-suffix-wave)
		 ("lbb" "left bottom bottom" tsc-suffix-wave)] ; note, no newline

		["Right Column\n"
		 ("rtt" "right top top" tsc-suffix-wave)
		 ("rtb" "right top bottom" tsc-suffix-wave)
		 ""
		 ("rbt" "right bottom top" tsc-suffix-wave)
		 ("rbb" "right bottom bottom\n" tsc-suffix-wave)]])

(tsc-layout-the-grid)

(transient-define-prefix tsc-layout-explicit-classes ()
  "Prefix with group class used to explicitly specify layout."
  [:class transient-row "Row"
	  ("l" "wave left" tsc-suffix-wave)
	  ("r" "wave right" tsc-suffix-wave)]
  [:class transient-column "Column"
	  ("t" "wave top" tsc-suffix-wave)
	  ("b" "wave bottom" tsc-suffix-wave)])

(tsc-layout-explicit-classes)

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
(transient-define-prefix tsc-stay-transient ()
  "Prefix where some suffixes do not exit."
  ["Exit or Not?"

   ;; this suffix will not exit after calling sub-prefix
   ("we" "wave & exit" tsc-wave-overridden)
   ("ws" "wave & stay" tsc-wave :transient t)])

(tsc-stay-transient)



(defun tsc--wave-override ()
  "Vanilla command used to override suffix's commands."
  (interactive)
    (message "This suffix was overridden.  I am what remains."))
(transient-define-suffix tsc-suffix-wave-macroed ()
  "Prefix that waves with macro-defined suffix."
  :transient t
  :key "T"
  :description "wave from macro definition"
  (interactive)
  (message "Waves from a macro definition at: %s" (current-time-string)))

(transient-define-prefix oqa-opensuse ()
  "Prefix that waves with overridden suffix behavior."
  [(tsc-suffix-wave-macroed
    :transient transient--do-call
    :key "stw"
    :description "TW status"
    :command oqa-status)])

(transient-define-prefix oqa-suse ()
  "Prefix that waves with overridden suffix behavior."
  [(tsc-suffix-wave-macroed
    :transient nil
    :key "O"
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
	   (number-to-string (gethash "unfinished" last-build)))))
  )

(list (get-nth-opensuse-build 0))

;; suffix cmds
;; https://magit.vc/manual/transient/Defining-Suffix-and-Infix-Commands.html
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

(transient-define-prefix oqa-transient ()
  "oqa Menu"
  ["OpenQA Instance"

   ;; this suffix will not exit after calling sub-prefix
   ("ooo" "openqa.opensuse.org" oqa-opensuse :transient t)
   ("osd" "openqa.suse.de" tsc-wave :transient t)])

(oqa-transient)

(transient-define-suffix tsc--suffix-interactive-string (user-input)
  "An interactive suffix that obtains string input from the user."
  (interactive "sPlease just tell me what you want!: ")
  (message "I think you want: %s" user-input))

(transient-define-suffix tsc--suffix-interactive-buffer-name (buffer-name)
  "An interactive suffix that obtains a buffer name from the user."
  (interactive "b")
  (message "You selected: %s" buffer-name))

(transient-define-prefix tsc-interactive-basic ()
  "Prefix with interactive user input."
  ["Interactive Command Suffixes"
   ("s" "enter string" tsc--suffix-interactive-string)
   ("b" "select buffer" tsc--suffix-interactive-buffer-name)])

(tsc-interactive-basic)

;; UI

(transient-define-suffix tsc-suffix-print-args (the-prefix-arg)
  "Report the PREFIX-ARG, prefix's scope, and infix values."
  :transient 'transient--do-call
  (interactive "P")
  (let ((args (transient-args (oref transient-current-prefix command)))
	(scope (oref transient-current-prefix scope)))
    (message "prefix-arg: %s \nprefix's scope value: %s \ntransient-args: %s"
	                  the-prefix-arg scope args)))
;; infix defined with a macro
(transient-define-argument tsc--exclusive-switches ()
  "This is a specialized infix for only selecting one of several values."
  :class 'transient-switches
  :argument-format "--%s-snowcone"
  :argument-regexp "\\(--\\(grape\\|orange\\|cherry\\|lime\\)-snowcone\\)"
  :choices '("grape" "orange" "cherry" "lime"))

(transient-define-prefix tsc-basic-infixes ()
  "Prefix that just shows off many typical infix types."
  ["Infixes"

   ;; from macro
   ("-e" "exclusive switches" tsc--exclusive-switches)

   ;; shorthand definitions
   ("-b" "switch with shortarg" ("-w" "--switch-short")) ; with :short-arg != :key
   ("-s" "switch" "--switch")
   ( "n" "no dash switch" "still works")
   ("-a" "argument" "--argument=" :prompt "Let's argue because: ")

   ;; a bit of inline EIEIO in our shorthand
   ("-n" "never empty" "--non-null=" :always-read t  :allow-empty nil
    :init-value (lambda (obj) (oset obj value "better-than-nothing")))

   ("-c" "choices" "--choice=" :choices (foo bar baz))]

  ["Show Args"
   ("s" "show arguments" tsc-suffix-print-args)])

(tsc-basic-infixes)

(transient-args transient-current-command)
(transient-arg-value "--argument-" (transient-args transient-current-command))
(transient-suffixes transient-current-command)

(transient-define-suffix tsc--read-prefix-scope ()
  "Read the scope of the prefix."
  :transient 'transient--do-call
  (interactive)
  (let ((scope (oref transient-current-prefix scope)))
    (message "scope: %s" scope)))

(transient-define-suffix tsc--double-scope-re-enter ()
  "Re-enter the current prefix with double the scope."
  ;; :transient 'transient--do-replace ; builds up the stack
  :transient 'transient--do-exit
  (interactive)
  (let ((scope (oref transient-current-prefix scope)))
    (if (numberp scope)
	(transient-setup transient-current-command nil nil :scope (* scope 2))
      (message (propertize (format "scope was non-numeric! %s" scope) 'face 'warning))
      (transient-setup transient-current-command))))

(transient-define-suffix tsc--update-scope-with-prefix-re-enter (new-scope)
  "Re-enter the prefix with double the scope."
  ;; :transient 'transient--do-replace ; builds up the stack
  :transient 'transient--do-exit ; do not build up the stack
  (interactive "P")
  (message "universal arg: %s" new-scope)
  (transient-setup transient-current-command nil nil :scope new-scope))

(transient-define-prefix tsc-scope (scope)
  "Prefix demonstrating use of scope."

  ;; note!  this is a location where we definitely had to use
  ;; `transient--prefix' or get the transient object from the tsc-scope symbol.
  ;; `transient-current-prefix' is not correct here!
  [:description (lambda () (format "Scope: %s" (oref transient--prefix scope)))
		[("r" "read scope" tsc--read-prefix-scope)
		 ("d" "double scope" tsc--double-scope-re-enter)
		 ("o" "update scope (use prefix argument)" tsc--update-scope-with-prefix-re-enter)]]
  (interactive "P")
  (transient-setup 'tsc-scope nil nil :scope scope))

;; Setting an interactive argument for `eval-last-sexp' is a little different
(let ((current-prefix-arg 4)) (call-interactively 'tsc-scope))

(tsc-scope)
;; Then press "C-u 4 o" to update the scope
;; Then d to double
;; Then r to read
;; ... and so on
;; C-g to exit

(defun tsc--animal-choices (_complete-me _predicate flag)
  "Programmed completion for animal choice.
_COMPLETE-ME: whatever the user has typed so far
_PREDICATE: function you should use to filter candidates (only nil seen so far)
FLAG: request for metadata (which can be disrespected)"

  ;; if you want to respect metadata requests, here's what the form might
  ;; look like, but no behavior was observed.
  (if (eq flag 'metadata)
      '(metadata . '((annotation-function . (lambda (c) "an annotation"))))

    ;; when not handling a metadata request from completions, use some
    ;; logic to generate the choices, possibly based on input or some time
    ;; / context sensitive process.  FLAG will be `t' when these are reqeusted.
    (if (eq 0 (random 2))
	'("fox" "kitten" "otter")
      '("ant" "peregrine" "zebra"))))

(transient-define-prefix tsc-choices-with-completions ()
  "Prefix with completions for choices."
  ["Arguments"
   ("-a" "Animal" "--animal="
    :always-read t ; don't allow unsetting, just read a new value
    :choices tsc--animal-choices)]
  ["Show Args"
   ("s" "show arguments" tsc-suffix-print-args)])

(tsc-choices-with-completions)

;;

(defun tsc--quit-cowsay ()
  "Kill the cowsay buffer and exit."
  (interactive)
  (kill-buffer "*cowsay*"))

(defun tsc--cowsay-buffer-exists-p ()
  "Visibility predicate."
  (not (equal (get-buffer "*cowsay*") nil)))

(transient-define-suffix tsc--cowsay-clear-buffer (&optional buffer)
  "Delete the *cowsay* buffer.  Optional BUFFER name."
  :transient 'transient--do-call
  :if 'tsc--cowsay-buffer-exists-p
  (interactive) ; todo look at "b" interactive code

  (save-excursion
    (let ((buffer (or buffer "*cowsay*")))
      (set-buffer buffer)
      (delete-region 1 (+ 1 (buffer-size))))))

(transient-define-suffix tsc--cowsay (&optional args)
  "Run cowsay."
  (interactive (list (transient-args transient-current-command)))
  (let* ((buffer "*cowsay*")
	 ;; TODO ugly
	 (cowmsg (if args (transient-arg-value "--message=" args) nil))
	 (cowmsg (if cowmsg (list cowmsg) nil))
	 (args (if args
		   (seq-filter
		    (lambda (s) (not (string-prefix-p "--message=" s))) args)
		 nil))
	 (args (if args
		   (if cowmsg
		       (append args cowmsg)
		     args)
		 cowmsg)))

    (when (tsc--cowsay-buffer-exists-p)
      (tsc--cowsay-clear-buffer))
    (apply #'call-process "cowsay" nil buffer nil args)
    (switch-to-buffer buffer)))

(transient-define-prefix tsc-cowsay ()
  "Say things with animals!"

					; only one kind of eyes is meaningful at a time
  :incompatible '(("-b" "-g" "-p" "-s" "-t" "-w" "-y"))

  ["Message"
   ("m" "message" "--message=" :always-read t)] ; always-read, so clear by entering empty string
  [["Built-in Eyes"
    ("b" "borg" "-b")
    ("g" "greedy" "-g")
    ("p" "paranoid" "-p")
    ("s" "stoned" "-s")
    ("t" "tired" "-t")
    ("w" "wired" "-w")
    ("y" "youthful" "-y")]
   ["Actions"
    ("c" "cowsay" tsc--cowsay :transient transient--do-call)
    ""
    ("d" "delete buffer" tsc--cowsay-clear-buffer)
    ("q" "quit" tsc--quit-cowsay)]])

(tsc-cowsay)

(defvar tsc-busy nil "Are we busy?")

(defun tsc--busy-p () "Are we busy?" tsc-busy)

(transient-define-suffix tsc--toggle-busy ()
  "Toggle busy."
  (interactive)
  (setf tsc-busy (not tsc-busy))
  (message (propertize (format "busy: %s" tsc-busy)
		       'face 'success)))

(tsc--toggle-busy)

(transient-define-prefix tsc-generated-child ()
  "Prefix that uses `setup-children' to generate single child."

  ["Replace this child"
   ;; Let's override the group's method
   :setup-children
   (lambda (_) ; we don't care about the stupid suffix

     ;; remember to return a list
     (list (transient-parse-suffix
	    transient--prefix
	    '("r" "replacement" (lambda ()
				  (interactive)
				  (message "okay!"))))))

   ("s" "haha stupid suffix" (lambda ()
			       (interactive)
			       (message "You should replace me!")))])

(tsc-generated-child)


;; Let's look at the layout
(let ((prefix-layout (plist-get (symbol-plist 'magit-dispatch) 'transient--layout)))

  (type-of prefix-layout) ; cons

  (listp prefix-layout) ; t

  (length prefix-layout) ; 3

  ;; Each group in the list is a vector
  (vectorp (car prefix-layout)) ; t

  (elt (car prefix-layout) 0) ; first element is a priority
  (elt (car prefix-layout) 1) ; second is a type name
  (elt (car prefix-layout) 2) ; contents & attributes

  ;; the attributes are key-value pairs used to create the class
  ;; instance when the transient is shown.

  ;; the nested contents will be lists of vectors for groups and
  ;; lists of lists for suffixes.

  )

;; A sample layout

([1 transient-column nil
    ((1 transient-suffix
        (:key "i" :description "Ignore" :command magit-gitignore))
     (1 transient-suffix
        (:key "I" :description "Init" :command magit-init))
     (1 transient-suffix
        (:key "j" :description "Jump to section" :command magit-status-jump :if-mode magit-status-mode))
     (1 transient-suffix
        (:key "j" :description "Display status" :command magit-status-quick :if-not-mode magit-status-mode)))])

  
