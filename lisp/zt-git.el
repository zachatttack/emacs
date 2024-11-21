(use-package git-link
  :config
  (global-set-key (kbd "C-c g l") 'git-link)
  )

(use-package magit)

(use-package git-gutter
  :config
  (setq git-gutter:modified-sign "|")
  (setq git-gutter:added-sign "|")
  (setq git-gutter:deleted-sign "|")
  (global-git-gutter-mode t)
  )

(provide 'zt-git)
