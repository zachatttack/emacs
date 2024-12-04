(defun zephyr-term ()
  "Opens ttyACM0 for zephyr terminal"
  (interactive)
(if (get-buffer buffer-name)
      ;; If the buffer exists, switch to it
      (switch-to-buffer buffer-name)
    ;; Otherwise, display a message
    (message "Buffer %s does not exist" buffer-name))
  (serial-term "/dev/ttyACM0" 115200 'term-line-mode)
  )

(defun zephyr-term (buffer-name)
  "Open a buffer with BUFFER-NAME if it exists, otherwise display a message."
  (if (get-buffer buffer-name)
      ;; If the buffer exists, switch to it
      (switch-to-buffer buffer-name)
    ;; Otherwise, display a message
    (message "Buffer %s does not exist" buffer-name)))

(use-package vterm)
(use-package multi-vterm)

(straight-use-package
 '(eat :type git
       :host codeberg
       :repo "akib/emacs-eat"
       :files ("*.el" ("term" "term/*.el") "*.texi"
               "*.ti" ("terminfo/e" "terminfo/e/*")
               ("terminfo/65" "terminfo/65/*")
               ("integration" "integration/*")
               (:exclude ".dir-locals.el" "*-tests.el"))))

(eat-eshell-mode)
(setq eshell-visual-commands '())

(provide 'zt-term)
