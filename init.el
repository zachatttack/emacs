(setq native-comp-speed 3)
(setq inhibit-startup-message t)
(setq visible-bell t)
(setq native-comp-async-report-warnings-errors nil)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room
(menu-bar-mode -1)            ; Disable the menu bar
(global-auto-revert-mode 1)

(use-package auto-compile
  :config (auto-compile-on-load-mode))
(setq load-prefer-newer t)

;; Revert Dired and other buffers
(setq global-auto-revert-non-file-buffers t)
(setq create-lockfiles nil)
(setq dired-dwim-target t)
(setq custom-file "~/.emacs.d/custom-file.el")
(load-file custom-file)

(fset 'yes-or-no-p 'y-or-n-p)

(setq comp-async-report-warnings-errors nil)

(set-face-attribute 'default nil :font "Iosevka NF" :height 200)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")
                         '("nongnu" . "https://elpa.nongnu.org/nongnu/")
			 ("melpa-stable" . "https://stable.melpa.org/packages/")
			 ))

(package-initialize)
(unless package-archive-contents
 (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
   (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package no-littering)
(setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
(setq make-backup-files nil)

(defun zt/evil-hook ()
  (dolist (mode '(custom-mode
                  eshell-mode
                  git-rebase-mode
                  erc-mode
                  vterm-mode
                  circe-server-mode
                  circe-chat-mode
                  circe-query-mode
                  sauron-mode
                  term-mode))
   (add-to-list 'evil-emacs-state-modes mode)))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump t)
  (setq evil-undo-system 'undo-tree)
  (setq evil-want-minibuffer nil)
  (setq evil-want-fine-undo t)
  (setq evil-split-window-below t)
  (setq evil-respect-visual-line-mode t)
  (setq evil-vsplit-window-right t)
  :hook (evil-mode . zt/evil-hook)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  ;;(define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal)
  (evil-set-initial-state 'git-commit-mode 'insert) ;; enter insert mode to edit a commit message
  ;;emacs-mode instead of insert mode
  ;; (defalias 'evil-insert-state 'evil-emacs-state)
  (define-key evil-emacs-state-map (kbd "<escape>") 'evil-normal-state)
  (setq evil-emacs-state-cursor '(bar . 1))
  )

(use-package evil-org
  :ensure t
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package evil-goggles
  :ensure t
  :init 
  (setq evil-goggles-duration 0.100)
  :config
  (evil-goggles-mode))

(use-package evil-collection
  :after evil
  :custom (evil-collection-setup-minibuffer nil)
  :config
  (evil-collection-init))

(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

;; (use-package kaolin-themes
;;  :config
;;   (load-theme 'kaolin-temple t)
;;  (kaolin-treemacs-theme))
 
(use-package catppuccin-theme
  :config
  (setq catppuccin-flavor 'mocha)
  )

(use-package spaceway-theme
  :ensure nil
  :load-path "lisp/spaceway/"
  :config
  (global-hl-line-mode t)
  (set-frame-parameter nil 'cursor-color "#dc322f")
  (add-to-list 'default-frame-alist '(cursor-color . "#dc322f"))

  ;; (when my/my-system
  ;;   (set-frame-parameter nil 'alpha-background 85)
  ;;   (add-to-list 'default-frame-alist '(alpha-background . 85)))

  ;; (load-theme 'spaceway t)
  (setenv "SCHEME" "dark")
  )
 

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(column-number-mode)
(dolist (mode '(org-mode-hook
                term-mode-hook
                vterm-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda() (display-line-numbers-mode 0))))

(global-display-line-numbers-mode t)
(setq display-line-numbers-type 'relative)
(dolist (mode '(
		term-mode-hook
		pdf-view-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  :config
  (custom-set-faces
   '(rainbow-delimiters-depth-1-face ((t (:inherit rainbow-delimiters-base-face :foreground "SlateBlue1"))))
   '(rainbow-delimiters-depth-2-face ((t (:inherit rainbow-delimiters-base-face :foreground "chartreuse4"))))
   '(rainbow-delimiters-depth-3-face ((t (:inherit rainbow-delimiters-base-face :foreground "medium orchid"))))
   '(rainbow-delimiters-depth-4-face ((t (:inherit rainbow-delimiters-base-face :foreground "HotPink1"))))
   '(rainbow-delimiters-depth-5-face ((t (:inherit rainbow-delimiters-base-face :foreground "SystemHilight"))))
   '(rainbow-delimiters-depth-6-face ((t (:inherit rainbow-delimiters-base-face :foreground "gray55"))))
   '(rainbow-delimiters-depth-7-face ((t (:inherit rainbow-delimiters-base-face :foreground "firebrick1"))))
   '(rainbow-delimiters-depth-8-face ((t (:inherit rainbow-delimiters-base-face :foreground "chartreuse2"))))
   '(rainbow-delimiters-depth-9-face ((t (:inherit rainbow-delimiters-base-face :foreground "purple3"))))
   ))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))


(use-package all-the-icons)
(use-package all-the-icons-completion
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))

(use-package general
  :config
  (general-auto-unbind-keys)
  ;; (general-define-key "C-w u" 'winner-undo :which-key "Undo window change")
  (general-define-key "M-j" 'next-error)
  (general-define-key "M-k" 'previous-error)
  (general-create-definer zt/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")) 

(global-set-key (kbd "<f5>") 'compile)

(zt/leader-keys
  "g"  '(:ignore g :which-key "git")
  ;; "p"  '(:ignore p :which-key "project")
  "p"  '(projectile-switch-project :which-key "open project")
  "gs" '(magit-status :which-key "Open Magit")
  "gb" '(magit-blame :which-key "Inline git blame")
  "gh" '(magit-log-buffer-file :which-key "Open git history for open buffer")
  "gl" '(magit-log :which-key "Open git log")
  ;; "gs" '((lambda () (interactive)(counsel-projectile-switch-project 13)):which-key "open magit for project")
  "oe" '(eshell :which-key "open eshell")
  "ot" '(multi-vterm-dedicated-toggle :which-key "toggle dedicated vterm")
  "od" '(dired-jump :which-key "open dired")
  "oi" 'my-open-init-file
  "SPC" '(projectile-find-file :which-key "search in project")
  "gt" 'eyebrowse-next-window-config
  "gT" 'eyebrowse-prev-window-config
  "gc" 'eyebrowse-close-window-config
  "g." 'eyebrowse-switch-to-window-config
  "." 'avy-goto-char-timer
  "b" '(:ignore t :wk "buffer")
  "b i" '(ibuffer :wk "Ibuffer")
  "f" '(:ignore t :wk "file")
  "f f" '(find-file t :wk "find file")
  "f c" '(my-open-init-file t :wk "init file")
  )

(use-package undo-fu)

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (eq system-type 'windows-nt)
    (when (file-directory-p "c:/work")
      (setq projectile-project-search-path '("c:/work")))
    (setq projectile-switch-project-action #'projectile-dired)
    )
  (when (eq system-type 'gnu/linux)
    (when (file-directory-p "~/repos")
      (setq projectile-project-search-path '("~/repos")))
    (setq projectile-switch-project-action #'projectile-dired)
    )
  )

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))
;; (setq magit-git-executable "~/.emacs.d/bin/git")

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :custom ((dired-listing-switches "-agho --group-directories-first"))
  :bind (("C-x C-j" . dired-jump))
  :config
  (use-package treemacs-icons-dired
    :if (display-graphic-p)
    :config (treemacs-icons-dired-mode))
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-single-up-directory
    "l" 'dired-single-buffer)
  (setq dired-kill-when-opening-new-dired-buffer t)
  )


(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "H" 'dired-hide-dotfiles-mode))

(use-package recentf
  :config
  (setq recentf-auto-cleanup 'never
        recentf-max-saved-items 1000
        recentf-save-file (concat user-emacs-directory ".recentf"))
  (recentf-mode t)
  :diminish nil)

(use-package git-gutter
  :config
  (setq git-gutter:modified-sign "|")
  (setq git-gutter:added-sign "|")
  (setq git-gutter:deleted-sign "|")
  (global-git-gutter-mode t)
  )

(set-default-coding-systems 'utf-8)

(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  )

(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (cmake "https://github.com/uyha/tree-sitter-cmake")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (make "https://github.com/alemuller/tree-sitter-make")
        (markdown "https://github.com/ikatyang/tree-sitter-markdown")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")
        (c "https://github.com/tree-sitter/tree-sitter-c")
        (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
        ))

(setq major-mode-remap-alist
      '((yaml-mode . yaml-ts-mode)
        (bash-mode . bash-ts-mode)
        (js2-mode . js-ts-mode)
        (typescript-mode . typescript-ts-mode)
        (json-mode . json-ts-mode)
        (css-mode . css-ts-mode)
        (python-mode . python-ts-mode)
        (c-mode . c-ts-mode)
        (c++-mode . c++-ts-mode)
        (c-or-c++-mode . c-or-c++-ts-mode)
        ))
(setq treesit-font-lock-level 4)

(use-package ripgrep)
(use-package hl-todo
       :ensure t
       :custom-face
       (hl-todo ((t (:inherit hl-todo :italic t))))
       :hook ((prog-mode . hl-todo-mode)
              (yaml-mode . hl-todo-mode)))
(global-hl-todo-mode)

(cond 
 ((string-match-p "WSL2" (shell-command-to-string "uname -a"))
		 (setq org-directory "/home/zach/org/roam"))
 ((eq system-type 'windows-nt)
     (setq org-directory "H:/zthomas/private/org"))
 (t
  (setq org-directory "/mnt/nas/org"))
 )

(setq path-to-ctags "C:/Users/zthomas/Documents/emacs-28.1/bin/ctags.exe")

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-switchb)

(eval-after-load "org"
  '(require 'ox-md nil t))

(use-package org-roam
  :after org
  :init (setq org-roam-v2-ack t) ;; Acknowledge V2 upgrade
  (setq org-roam-dailies-directory "daily/")
  (setq org-roam-dailies-capture-templates
        '(("d" "default" entry "** %?" :if-new
           (file+head+olp "%<%G-W%V>.org" "#+title: %<%G-W%V>\n"
                          ("%<%A %Y-%m-%d>")))))
  :custom
  (org-roam-directory (file-truename org-directory))
  :bind (("C-c n f" . org-roam-node-find)
         
	 ("C-c n r" . org-roam-node-random)		    
	 (:map org-mode-map
	       (("C-c n i" . org-roam-node-insert)
		("C-c n o" . org-id-get-create)
		("C-c n t" . org-roam-tag-add)
		("C-c n a" . org-roam-alias-add)
		("C-c n l" . org-roam-buffer-toggle))))
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  :config
  (require 'org-roam-dailies)
  (org-roam-setup)
  )

(setq org-roam-dailies-directory "worklog/")

(use-package org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; ;; Improve org mode looks
;; (setq-default org-startup-indented t
;;               org-pretty-entities t
;;               org-use-sub-superscripts "{}"
;;               org-hide-emphasis-markers t
;;               org-startup-with-inline-images t
;;               org-image-actual-width '(300))

(use-package org-appear
  :hook
  (org-mode . org-appear-mode))

(use-package org-modern
  :hook
  (org-mode . global-org-modern-mode)
  ;; :custom
  ;; (org-modern-keyword nil)
  ;; (org-modern-checkbox nil)
  ;; (org-modern-table nil)
  )

(use-package org-download)
(setq org-startup-with-inline-images t)

(setq org-todo-keywords
      '((sequence "TODO" "WAITING" "|" "DONE" )))

(setq browse-url-browser-function 'eww-browse-url
    shr-use-colors nil
    shr-bullet "• "
    shr-folding-mode t
    eww-search-prefix "https://duckduckgo.com/html?q="
    url-privacy-level '(email agent cookies lastloc))
(use-package pdf-tools
  :config
  (pdf-tools-install)
  (add-hook 'pdf-tools-enabled-hook 'pdf-view-midnight-minor-mode)
  ;;(add-hook 'pdf-view-mode-hook (lambda () (blink-cursor-mode -1)))
  (setq-default pdf-view-display-size 'fit-page)
  )
(blink-cursor-mode -1)

(use-package savehist
  :init
  (savehist-mode))


(setq scroll-margin 5)
(setq scroll-conservatively 100)

(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
              (ggtags-mode 1))))

(advice-add 'ggtags-prev-mark :override
	    (lambda () (pop-tag-mark)))

(use-package dumb-jump
  :config
  (setq dumb-jump-force-searcher 'rg)
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read)
  )

;; (when (window-system)
;;  (set-frame-font "Iosevka NF"))
;; (set-face-attribute 'default nil :font "Iosevka NF" :height 160)
;; (add-to-list 'default-frame-alist '(font . "Iosevka NF"))

(let ((alist '((33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
               (35 . ".\\(?:###\\|##\\|_(\\|[#(?[_{]\\)")
               (36 . ".\\(?:>\\)")
               (37 . ".\\(?:\\(?:%%\\)\\|%\\)")
               (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
               (42 . ".\\(?:\\(?:\\*\\*/\\)\\|\\(?:\\*[*/]\\)\\|[*/>]\\)")
               (43 . ".\\(?:\\(?:\\+\\+\\)\\|[+>]\\)")
               (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
               (46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=-]\\)")
               (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
               (48 . ".\\(?:x[a-zA-Z]\\)")
               (58 . ".\\(?:::\\|[:=]\\)")
               (59 . ".\\(?:;;\\|;\\)")
               (60 . ".\\(?:\\(?:!--\\)\\|\\(?:~~\\|->\\|\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[*$+~/<=>|-]\\)")
               (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
               (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
               (63 . ".\\(?:\\(\\?\\?\\)\\|[:=?]\\)")
               (91 . ".\\(?:]\\)")
               (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
               (94 . ".\\(?:=\\)")
               (119 . ".\\(?:ww\\)")
               (123 . ".\\(?:-\\)")
               (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
               (126 . ".\\(?:~>\\|~~\\|[>=@~-]\\)")
               )
             ))
  (dolist (char-regexp alist)
    (set-char-table-range composition-function-table (car char-regexp)
                          `([,(cdr char-regexp) 0 font-shape-gstring]))))

(use-package idle-highlight-mode
  :config (setq idle-highlight-idle-time 0.4)
  (global-idle-highlight-mode)
  )


; Default colours are too light (to see colour names do M-x list-colors-display
; and to see faces do M-x list-faces-display):

;(use-package doom-themes
 ; :init 
  ;  ;; :config (load-theme 'doom-one t)
   ; ;; (add-hook 'server-after-make-frame-hook (lambda () (load-theme 'doom-one t)))
    ;)

(use-package spacemacs-theme
  :defer t
  ;; (load-theme 'spacemacs-dark t)
  )


(add-to-list 'auto-mode-alist '("\\.dsc" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.inf" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.dec" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.dsc" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.fdf" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.vfr" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.hfr" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.robot" . python-mode))

(add-to-list 'auto-mode-alist '("\\.bin\\'" . hexl-mode))
(add-to-list 'auto-mode-alist '("\\.cu\\'" . c++-mode))

(add-to-list 'auto-mode-alist '("\\.dts\\'" . devicetree-ts-mode))
(add-to-list 'auto-mode-alist '("\\.dtsi\\'" . devicetree-ts-mode))
(add-to-list 'auto-mode-alist '("\\.overlay\\'" . devicetree-ts-mode))

(add-to-list 'auto-mode-alist '("\\.defconfig\\'" . kconfig-mode))
(add-to-list 'auto-mode-alist '("\\.board\\'" . kconfig-mode))

(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-ts-mode))
(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-ts-mode))

(setq tramp-default-method "sshx")
;; Dired stuff

;; Open dired folders in same buffer
(put 'dired-find-alternate-file 'disabled nil)
;; Sort Dired buffers
(setq dired-listing-switches "-agho --group-directories-first")
;; Copy and move files netween dired buffers
(setq dired-dwim-target t)
;;trash instead of delete
(setq delete-by-moving-to-trash t)
;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

(when (eq system-type 'windows-nt)
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "C:/Program Files/Google/Chrome/Application/chrome.exe")
)
(when (eq system-type 'gnu/linux)
  (setq browse-url-browser-function 'browse-url-generic
        browse-url-generic-program (if (string-match-p "Windows" (getenv "PATH"))
                                       "/mnt/c/Program Files/Mozilla Firefox/firefox.exe"
                                     "firefox"))
  )
(global-set-key (kbd "C-x C-b") 'ibuffer)

(add-hook 'ibuffer-mode-hook
	  #'(lambda ()
	     (ibuffer-auto-mode 1)))


(use-package async)

;; (when (eq system-type 'windows-nt)
;;   (setq org-agenda-files '("H:/zthomas/private/org"))
;;   )
;; (when (eq system-type 'gnu/linux)
;;   (setq org-agenda-files '("/mnt/nas/org"))
;;   )
(setq org-agenda-files '("/mnt/nas/org/agenda/"))

(setq ibuffer-saved-filter-groups
          (quote (("default"
                   ("vterm" (mode . vterm-mode))
                   ("shells" (mode . shell-mode))
                   ("dired" (mode . dired-mode))
                   ("org" (mode . org-mode))
                   ("emacs" (or
                             (name . "^\\*scratch\\*$")
                             (name . "^\\*Messages\\*$")))
		   ("pdfs" (name . "\\.pdf"))
                   ))))

(require 'ibuf-ext)

 (add-hook 'ibuffer-mode-hook
              (lambda ()
                (ibuffer-switch-to-saved-filter-groups "default")))
(when (eq system-type 'windows-nt)
  (setq org-capture-templates
	'(("t" "Todo" entry (file+headline "H:/zthomas/private/org/GTD.org" "Tasks")
	   "* TODO %?\n  %i\n")
	  ("j" "Journal" entry (file+datetree "h:/zthomas/private/org/journal.org")
	   "* %?\nEntered on %U\n  %i\n  %a")))
  )
(when (eq system-type 'gnu/linux)
  (setq org-capture-templates
	'(("t" "Todo" entry (file+headline "/mnt/nas/org/GTD.org" "Tasks")
	   "* TODO %?\n  %i\n")
	  ("j" "Journal" entry (file+datetree "/mnt/nas/org/journal.org")
	   "* %?\nEntered on %U\n  %i\n  %a")))
  )

(use-package nix-mode
  :mode "\\.nix\\'")

;; (use-package perfect-margin
;;   :config
;;   (setq perfect-margin-visible-width 128)
;;   (perfect-margin-mode 1)
;;   )

(winner-mode)

;;(use-package sticky-shell
  ;;:config
  ;;(sticky-shell-global-mode)
  ;;)
;;
               ;;;;;;Vertico 
(use-package vertico
  :init
  (vertico-mode)
  (setq vertico-scroll-margin 0)
  (setq vertico-count 20)
  (setq vertico-resize t)
  (setq vertico-cycle t)
  )

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  ;; Either bind `marginalia-cycle' globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init configuration is always executed (Not lazy!)
  :init

  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))

(use-package savehist
  :init
  (savehist-mode))

;; Optionally use the `orderless' completion style.
(use-package orderless
  :custom
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless partial-completion basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion))))
  (orderless-component-separator 'orderless-escapable-split-on-space)
  (orderless-matching-styles
   '(orderless-literal
     orderless-prefixes
     orderless-initialism
     orderless-regexp
     orderless-flex
     ;; orderless-strict-leading-initialism
     ;; orderless-strict-initialism
     ;; orderless-strict-full-initialism
     ;; orderless-without-literal          ; Recommended for dispatches instead
     ))
  )


(use-package corfu
  :after orderless
  ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))
  
  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since Dabbrev can be used globally (M-/).
  ;; See also `corfu-excluded-modes'.
  :init
  (global-corfu-mode))

(use-package cape
  ;; Bind dedicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  ;; :bind (("C-c p p" . completion-at-point) ;; capf
  ;;        ("C-c p t" . complete-tag)        ;; etags
  ;;        ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
  ;;        ("C-c p h" . cape-history)
  ;;        ("C-c p f" . cape-file)
  ;;        ("C-c p k" . cape-keyword)
  ;;        ("C-c p s" . cape-symbol)
  ;;        ("C-c p a" . cape-abbrev)
  ;;        ("C-c p l" . cape-line)
  ;;        ("C-c p w" . cape-dict)
  ;;        ("C-c p \\" . cape-tex)
  ;;        ("C-c p _" . cape-tex)
  ;;        ("C-c p ^" . cape-tex)
  ;;        ("C-c p &" . cape-sgml)
  ;;        ("C-c p r" . cape-rfc1345))
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  ;; NOTE: The order matters!
  (add-to-list 'completion-at-point-functions #'cape-history)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
)

(use-package consult)

(use-package kind-icon
  :ensure t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)
  (setq cursor-type 'bar)
  (global-set-key (kbd "M-o") 'other-window)

  ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
  ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete))

(defun zt-toggle-window-dedication ()
  "Toggles window dedication in the selected window."
  (interactive)
  (set-window-dedicated-p (selected-window)
     (not (window-dedicated-p (selected-window)))))

(add-hook 'shell-mode-hook 'compilation-shell-minor-mode)

(fset 'perl-mode 'cperl-mode)
(setq cperl-invalid-face nil)

;; (modify-syntax-entry ?_ "w")

;; Define the whitespace style.
(setq-default whitespace-style
              '(face spaces empty tabs newline trailing space-mark tab-mark newline-mark))
;; Whitespace color corrections.
(require 'color)
(let* ((ws-lighten 30) ;; Amount in percentage to lighten up black.
       (ws-color (color-lighten-name "#000000" ws-lighten)))
  (custom-set-faces
   `(whitespace-newline                ((t (:foreground ,ws-color))))
   `(whitespace-missing-newline-at-eof ((t (:foreground ,ws-color))))
   `(whitespace-space                  ((t (:foreground ,ws-color))))
   `(whitespace-space-after-tab        ((t (:foreground ,ws-color))))
   `(whitespace-space-before-tab       ((t (:foreground ,ws-color))))
   `(whitespace-tab                    ((t (:foreground ,ws-color))))
   `(whitespace-trailing               ((t (:foreground ,ws-color))))))
;; Make these characters represent whitespace.
(setq-default whitespace-display-mappings
      '(
        ;; space -> · else .
        (space-mark 32 [183] [46])
        ;; new line -> ¬ else $
        (newline-mark ?\n [172 ?\n] [36 ?\n])
        ;; carriage return (Windows) -> ¶ else #
        (newline-mark ?\r [182] [35])
        ;; tabs -> » else >
        (tab-mark ?\t [187 ?\t] [62 ?\t])))

(use-package undo-tree
  :ensure t
  :init
  (global-undo-tree-mode))

(use-package yasnippet
  :ensure t
  :hook ((text-mode
          prog-mode
          conf-mode
          snippet-mode) . yas-minor-mode-on)
  :init
  (setq yas-snippet-dir "~/.emacs.d/snippets"))

(use-package yasnippet-snippets)

(use-package beacon
  :config
  (beacon-mode 1)
  )

(setq c-set-offset 2)
(setq-default tab-width 4)

(setq-default indent-tabs-mode nil)
(setq-default tab-always-indent 'complete)

(use-package capf-autosuggest
  :config
  (add-hook 'comint-mode-hook #'capf-autosuggest-mode)
  (add-hook 'eshell-mode-hook #'capf-autosuggest-mode)
  )

(setq tab-bar-close-button-show nil
      tab-bar-new-button-show nil)

(use-package virtualenvwrapper
  :ensure t
  :config
  (venv-initialize-interactive-shells)
  (venv-initialize-eshell)
  )

(use-package elfeed
  :config
  (setq elfeed-db-directory (expand-file-name "elfeed" user-emacs-directory)
        elfeed-show-entry-switch 'display-buffer)
  (define-key elfeed-show-mode-map (kbd "n") 'meow-next)
  :bind
  ("C-x w" . elfeed ))

(use-package elfeed-org
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files (list (expand-file-name "elfeed.org" user-emacs-directory)))
  )

(use-package avy
  :ensure t
  :bind ("M-s" . avy-goto-char-timer)
  )

(when (eq system-type 'windows-nt)
  (setq consult-find-args "find . -not ( -wholename `*/.*` -prune )")
  )

(use-package tex
  :ensure auctex)

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package dimmer
  :init
  (dimmer-mode -1))

(use-package focus
  )

(defvar ediff-do-hexl-diff nil
  "variable used to store trigger for doing diff in hexl-mode")
(defadvice ediff-files-internal (around ediff-files-internal-for-binary-files activate)
  "catch the condition when the binary files differ

the reason for catching the error out here (when re-thrown from the inner advice)
is to let the stack continue to unwind before we start the new diff
otherwise some code in the middle of the stack expects some output that
isn't there and triggers an error"
  (let ((file-A (ad-get-arg 0))
        (file-B (ad-get-arg 1))
        ediff-do-hexl-diff)
    (condition-case err
        (progn
          ad-do-it)
      (error
       (if ediff-do-hexl-diff 
           (let ((buf-A (find-file-noselect file-A))
                 (buf-B (find-file-noselect file-B)))
             (with-current-buffer buf-A
               (hexl-mode 1))
             (with-current-buffer buf-B
               (hexl-mode 1))
             (ediff-buffers buf-A buf-B))
         (error (error-message-string err)))))))

(defadvice ediff-setup-diff-regions (around ediff-setup-diff-regions-for-binary-files activate)
  "when binary files differ, set the variable "
  (condition-case err
      (progn
        ad-do-it)
    (error
     (setq ediff-do-hexl-diff
           (and (string-match-p "^Errors in diff output.  Diff output is in.*"
                                (error-message-string err))
                (string-match-p "^\\(Binary \\)?[fF]iles .* and .* differ"
                                (buffer-substring-no-properties
                                 (line-beginning-position)
                                 (line-end-position)))
                (y-or-n-p "The binary files differ, look at the differences in hexl-mode? ")))
     (error (error-message-string err)))))

(windmove-default-keybindings 'shift)
(setq framemove-hook-into-windmove t)
(define-key evil-normal-state-map (kbd "H") 'windmove-left)
(define-key evil-normal-state-map (kbd "L") 'windmove-right)
(define-key evil-normal-state-map (kbd "J") 'windmove-down)
(define-key evil-normal-state-map (kbd "K") 'windmove-up)

(use-package git-link
  :config
  (global-set-key (kbd "C-c g l") 'git-link)
  )

(defun my-open-init-file ()
  "Open the init file."
  (interactive)
  (find-file user-init-file))

;; (add-to-list 'default-frame-alist '(fullscreen . maximized))

(use-package rg
  :config
  (rg-enable-default-bindings)
)

(require 'ob-eshell)
(require 'ob-octave)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (octave . t)
   (latex . t)
   (plantuml . t)
   ))

(require 'ox-latex)
(add-to-list 'org-latex-packages-alist '("" "minted"))
(setq org-latex-listings 'minted)

(setq org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

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

(defun zt/setup-appearance (frame)
  (with-selected-frame frame
    (remove-hook 'after-make-frame-functions 'zt/setup-appearance)
    (catppuccin-reload)
    ))

(if (daemonp)
    (add-hook 'after-make-frame-functions 'zt/setup-appearance)
  (zt/setup-appearance (car (frame-list)))
  )

(setq shr-max-image-proportion 0.8)
(setq org-agenda-span 21)

(setq org-deadline-warning-days 21)

(use-package lsp-mode
  :custom
  (lsp-completion-provider :none) ;; we use Corfu!
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-clients-clangd-args '("-j=4" "--log=verbose" ))
  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          ;; '(flex))) ;; Configure flex
          '(orderless))) ;; Configure orderless
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (c-ts-mode . lsp)
         (elisp-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration)
         (lsp-completion-mode . my/lsp-mode-setup-completion)
         )
  :commands lsp)

;; optionally
(use-package lsp-ui
  :commands lsp-ui-mode
  :custom
  (lsp-ui-doc-position 'bottom)
  )

;; needed for lsp-sideline
(use-package flycheck)

;; Sample executable configuration
(use-package plantuml-mode
  :init
  (setq plantuml-executable-path "/usr/bin/plantuml")
  (setq plantuml-default-exec-mode 'executable)
  ;; Enable plantuml-mode for PlantUML files
  (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))
  (add-to-list
  'org-src-lang-modes '("plantuml" . plantuml))
  (setq org-plantuml-exec-mode 'plantuml)
  )

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
(use-package multi-vterm
	:config
	(add-hook 'vterm-mode-hook
			(lambda ()
			(setq-local evil-insert-state-cursor 'box)
			(evil-insert-state)))
	(define-key vterm-mode-map [return]                      #'vterm-send-return)

	(setq vterm-keymap-exceptions nil)
	(evil-define-key 'insert vterm-mode-map (kbd "C-e")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-f")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-a")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-v")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-b")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-w")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-u")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-d")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-n")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-m")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-p")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-j")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-k")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-r")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-t")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-g")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-c")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-SPC")    #'vterm--self-insert)
	;; (evil-define-key 'normal vterm-mode-map (kbd "C-d")      #'vterm--self-insert)
	(evil-define-key 'normal vterm-mode-map (kbd ",c")       #'multi-vterm)
	(evil-define-key 'normal vterm-mode-map (kbd ",n")       #'multi-vterm-next)
	(evil-define-key 'normal vterm-mode-map (kbd ",p")       #'multi-vterm-prev)
	(evil-define-key 'normal vterm-mode-map (kbd "i")        #'evil-insert-resume)
	(evil-define-key 'normal vterm-mode-map (kbd "o")        #'evil-insert-resume)
	(evil-define-key 'normal vterm-mode-map (kbd "<return>") #'evil-insert-resume))

(use-package ace-window
  :bind (("M-o" . ace-window))
  :custom
  (aw-scope 'global)
  (aw-keys '(?t ?s ?n ?e ?r ?i ?a ?k o))
  (aw-minibuffer-flags t)
  )
(setq initial-buffer-choice (lambda () (get-buffer-create dashboard-buffer-name)))
