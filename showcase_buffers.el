(progn
  (save-current-buffer
    (set-buffer "scratch")
    (message "Current buffer: %s" (current-buffer)))
  (current-buffer))

(progn
  (with-current-buffer "*scratch*"
    (message "Current buffer: %s" (current-buffer)))
  (current-buffer))



