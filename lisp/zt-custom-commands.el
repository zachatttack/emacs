
(defvar current-date-format "%Y-%m-%d"
  "Format of date to insert with `insert-current-date-time' func
See help of `format-time-string' for possible replacements")

(defvar current-time-format "%a %H:%M:%S"
  "Format of date to insert with `insert-current-time' func.
Note the weekly scope of the command's precision.")

(defun insert-current-date ()
  "insert the current date and time into current buffer.
Uses `current-date-time-format' for the formatting the date/time."
       (interactive)
       (insert (format-time-string current-date-format (current-time)))
       )

(defun insert-current-time ()
  "insert the current time (1-week scope) into the current buffer."
       (interactive)
       (insert (format-time-string current-time-format (current-time)))
       (insert "\n")
       )

(global-set-key "\C-c\C-d" 'insert-current-date)
(global-set-key "\C-c\C-t" 'insert-current-time)






(defun zt/my-open-init-file ()
  "Open the init file."
  (interactive)
  (find-file user-init-file))

(defun newline-without-break-of-line ()
  "1. move to end of the line.
2. open new line and move to new line"
  (interactive)
  (end-of-line)
  (open-line 1)
  (right-char)
  (indent-according-to-mode))

(defun newline-above-without-break-of-line ()
  "1. move to end of the line.
2. open new line and move to new line"
  (interactive)
  (end-of-line 0)
  (open-line 1)
  (right-char)
  (indent-according-to-mode))


(global-set-key (kbd "<C-return>") 'newline-without-break-of-line)
(global-set-key (kbd "<C-S-return>") 'newline-above-without-break-of-line)

(global-set-key (kbd "C-1") 'delete-other-windows)
(global-set-key (kbd "C-2") 'split-window-below)
(global-set-key (kbd "C-3") 'split-window-right)


(provide 'zt-custom-commands)
