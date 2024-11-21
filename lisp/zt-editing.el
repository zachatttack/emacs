(use-package multiple-cursors
  :config
  (setq mc/always-run-for-all t)
  :bind
  (("C-S-J" . mc/mark-all-dwim)
  ("C-<" . mc/mark-all-like-this)
  ("C->" . mc/mark-next-like-this)))


(use-package expand-region
  :bind ("M-'" . er/expand-region))

(global-set-key (kbd "M-i") 'imenu)


(use-package avy
  :ensure t
  :bind ("M-s" . avy-goto-char-timer)
  :config
  (setq avy-timeout-seconds 1)
  )


(provide 'zt-editing)
