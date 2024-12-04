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
(use-package eat)


(provide 'zt-term)
