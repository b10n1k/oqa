(define-transient-command hrm-customize-transient ()
  "Customize Emacs settings"
  ["Customize Emacs settings"
   ["Global"
    ("C" "Customize" customize)
    ("B" "Browse" customize-browse)
    ("A" "Apropos" customize-apropos)
    ]
   ["Basic"
    ("g" "Group" customize-group)
    ("o" "Option" customize-option)
    ("v" "Option" customize-option)
    ("f" "Face" customize-face)
    ]
   ["Other Window"
    ("4g" "Group" customize-group-other-window)
    ("4o" "Option" customize-option-other-window)
    ("4v" "Option" customize-option-other-window)
    ("4f" "Face" customize-face-other-window)
    ]
   ]
  ["Advanced"
   ["Apropos"
    ("aa" "Everything" customize-apropos)
    ("ag" "Groups" customize-apropos-groups)
    ("ao" "Options" customize-apropos-options)
    ("af" "Faces" customize-apropos-faces)
    ]
   ["Search"
    ("n" "Since Version" customize-changed-options)
    ("u" "Unsaved" customize-unsaved)
    ("s" "Saved" customize-saved)
    ("r" "Rogue" customize-rogue) ; set outside custom
    ]
   ["Themes"
    ("t" "Theme" customize-themes)
    ("T" "Create Theme" customize-create-theme)
    ]
   ]
  )
(global-set-key (kbd "s-,") 'hrm-customize-transient)

(define-transient-command hrm-ediff-transient ()
  "Launch Ediff in all it's variants"
  ["Ediff"
   ["2 Way"
    ("b" "Buffers" ediff-buffers)
    ("f" "Files" ediff-files)
    ("d" "Directories" ediff-directories)
    ("c" "Buffer vs File" ediff-current-file)
    ("~" "File vs Backup" ediff-backup)
    ]
   ["3 Way"
    ("3b" "Buffers" ediff-buffers3)
    ("3f" "Files" ediff-files3)
    ("3d" "Directories" ediff-directories3)
    ]
   ["Patches"
    ("pb" "Buffer" ediff-patch-buffer)
    ("pf" "File" ediff-patch-file)
    ]
   ["Regions"
    ("rl" "Linewise" ediff-regions-linewise)
    ("rw" "Wordwise" ediff-regions-wordwise)
    ]
   ["Windows"
    ("wl" "Linewise" ediff-windows-linewise)
    ("ww" "Wordwise" ediff-windows-wordwise)
    ]
   ]
  )
(global-set-key (kbd "s-e") 'hrm-ediff-transient)

(define-transient-command hrm-help-transient ()
  "Help commands that I use. A subset of C-h with others thrown in."
  ["Help Commands"
   ["Mode & Bindings"
    ("m" "Mode" describe-mode)
    ("b" "Major Bindings" which-key-show-full-major-mode)
    ("B" "Minor Bindings" which-key-show-full-minor-mode-keymap)
    ("d" "Descbinds" counsel-descbinds)
    ("t" "Top Bindings  " which-key-show-top-level)
    ]
   ["Describe"
    ("C" "Command" helpful-command)
    ("f" "Function" helpful-callable)
    ("v" "Variable" helpful-variable)
    ("k" "Key" helpful-key)
    ("c" "Key Briefly" describe-key-briefly)
    ]
   ["Info on"
    ("C-c" "Emacs Command" Info-goto-emacs-command-node)
    ("C-f" "Function" counsel-info-lookup-symbol) ; s for symbol?
    ("C-v" "Variable" counsel-info-lookup-symbol) ; . for symbol?
    ("C-k" "Emacs Key" Info-goto-emacs-key-command-node)
    ]
   ["Goto Source"
    ("L" "Library" find-library-other-frame)
    ("F" "Function" find-function-other-frame)
    ("V" "Variable" find-variable-other-frame)
    ("K" "Key" find-function-on-key-other-frame)
    ]
   ]
  [
   ["Internals"
    ("I" "Input Method" describe-input-method)
    ("G" "Language Env" describe-language-environment)
    ("S" "Syntax" describe-syntax)
    ("O" "Coding System" describe-coding-system)
    ("C-o" "Coding Brief" describe-current-coding-system-briefly)
    ("T" "Display Table" describe-current-display-table)
    ("e" "Echo Messages" view-echo-area-messages)
    ("l" "Lossage" view-lossage)
    ]
   ["Describe"
    ("s" "Symbol" helpful-symbol)
    ("." "At Point   " helpful-at-point)
    ("C-f" "Face" counsel-describe-face)
    ("w" "Where Is" where-is)
    ("=" "Position" what-cursor-position)
    ]
   ["Info Manuals"
    ("C-i" "Info" info)
    ("C-4" "Other Window " info-other-window)
    ("C-e" "Emacs" info-emacs-manual)
    ("C-l" "Elisp" info-elisp-manual)
    ]
   ["External"
    ("W" "Dictionary" lookup-word-at-point)
    ("D" "Dash" dash-at-point)
    ]
   ]
  )
(global-set-key (kbd "C-S-h") 'hrm-help-transient)
