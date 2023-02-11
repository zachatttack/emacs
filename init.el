(setq inhibit-startup-message t)
(setq visible-bell t)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room
(menu-bar-mode -1)            ; Disable the menu bar
(global-auto-revert-mode 1)

;; Revert Dired and other buffers
(setq global-auto-revert-non-file-buffers t)
(setq create-lockfiles nil)
(setq dired-dwim-target t)
(setq custom-file "~/.emacs.d/custom-file.el")
(load-file custom-file)


(set-face-attribute 'default nil :font "Fira Code" :height 130)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")
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

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(column-number-mode)
(global-display-line-numbers-mode t)
(setq display-line-numbers-type 'relative)
(dolist (mode '(
		term-mode-hook
		pdf-view-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))


(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))


(use-package all-the-icons)

(use-package general
  :config
  (general-auto-unbind-keys)
  (general-define-key "C-w u" 'winner-undo :which-key "Undo window change")
  (general-create-definer zt/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")) 
 
  (zt/leader-keys
    "t"  '(:ignore t :which-key "toggles")
    "g"  '(:ignore g :which-key "git")
    "p"  '(:ignore p :which-key "project")
    "f"  '(:ignore f :which-key "file")
    "pp"  '(projectile-switch-project :which-key "open project")
    "gs" '(magit-status :which-key "Open Magit")
    "gb" '(magit-blame :which-key "Inline git blame")
    "gh" '(magit-log-buffer-file :which-key "Open git history for open buffer")
    "gl" '(magit-log :which-key "Open git log")
    ;; "gs" '((lambda () (interactive)(counsel-projectile-switch-project 13)):which-key "open magit for project")
    "sr" '(ripgrep-regexp :which-key "ripgrep")
    "oe" '(eshell :which-key "open eshell")
    "os" '(shell :which-key "open shell")
    "od" '(dired-jump :which-key "open dired")
    "SPC" '(projectile-find-file :which-key "search in project")
    "bb" '(ibuffer :which-key "buffers")
    "bm" '(bookmark-jump :which-key "jump to bookmark")
    "bo" '(previous-buffer :which-key "jump to previous buffer")
    )


(defun zt/evil-hook ()
  (dolist (mode '(custom-mode
                  eshell-mode
                  git-rebase-mode
                  erc-mode
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
  (setq evil-undo-system 'undo-fu)
  (setq evil-want-minibuffer nil)
  :hook (evil-mode . zt/evil-hook)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  ;;(define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

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
    (when (file-directory-p "~/")
      (setq projectile-project-search-path '("~/")))
    (setq projectile-switch-project-action #'projectile-dired)
    )
  )

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :custom ((dired-listing-switches "-agho --group-directories-first"))
  :bind (("C-x C-j" . dired-jump))
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-single-up-directory
    "l" 'dired-single-buffer))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-single)

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "H" 'dired-hide-dotfiles-mode))

(use-package company
  :config
  (global-company-mode t)
  (setq company-idle-delay 0.0)
  )

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



(defun create-tags (dir-name)
    "Create tags file."
    (interactive "DDirectory: ")
    (shell-command
     (format "%s -f TAGS -e -R %s" path-to-ctags (directory-file-name dir-name)))
  )

;; (autoload 'turn-on-ctags-auto-update-mode "ctags-update" "turn on 'ctags-auto-update-mode'." t)
;; (add-hook 'c-mode-common-hook  'turn-on-ctags-auto-update-mode)

(use-package tree-sitter)
(use-package tree-sitter-langs)
(tree-sitter-require 'c)
(global-tree-sitter-mode)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
(use-package ripgrep)
(use-package hl-todo
       :ensure t
       :custom-face
       (hl-todo ((t (:inherit hl-todo :italic t))))
       :hook ((prog-mode . hl-todo-mode)
              (yaml-mode . hl-todo-mode)))
(global-hl-todo-mode)

(when (eq system-type 'windows-nt)
  (setq org-directory "H:/zthomas/private/org")
  )
(when (eq system-type 'gnu/linux)
  (setq org-directory "/mnt/nas/org")
  )


(setq path-to-ctags "C:/Users/zthomas/Documents/emacs-28.1/bin/ctags.exe")

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-switchb)

(use-package org-roam
  :after org
  :init (setq org-roam-v2-ack t) ;; Acknowledge V2 upgrade
  :custom
  (org-roam-directory (file-truename org-directory))
  :config
  (org-roam-setup)
  :bind (("C-c n f" . org-roam-node-find)
	 ("C-c n r" . org-roam-node-random)		    
	 (:map org-mode-map
	       (("C-c n i" . org-roam-node-insert)
		("C-c n o" . org-id-get-create)
		("C-c n t" . org-roam-tag-add)
		("C-c n a" . org-roam-alias-add)
		("C-c n l" . org-roam-buffer-toggle)))))

(use-package org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

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
;;(define-key evil-normal-state-map (kbd "C-c C-c") 'evil-normal-state)
(define-key evil-insert-state-map (kbd "C-c") 'evil-normal-state)

(use-package savehist
  :init
  (savehist-mode))


(setq scroll-margin 5)
(setq scroll-conservatively 100)

(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
              (ggtags-mode 1))))

(when (window-system)
  (set-frame-font "Fira Code"))
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

(add-hook 'evil-visual-state-entry-hook
	  (lambda () (global-idle-highlight-mode -1))
	  )
(add-hook 'evil-visual-state-exit-hook
	  (lambda () (global-idle-highlight-mode))
	  )

(advice-add 'ggtags-prev-mark :override
	    (lambda () (pop-tag-mark)))
; Default colours are too light (to see colour names do M-x list-colors-display
; and to see faces do M-x list-faces-display):

(use-package doom-themes
  :init 
    ;; :config (load-theme 'doom-one t)
    ;; (add-hook 'server-after-make-frame-hook (lambda () (load-theme 'doom-one t)))
    )

(use-package spacemacs-theme
  :defer t
  )

(load-theme 'spacemacs-dark t)

(add-to-list 'auto-mode-alist '("\\.dsc" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.inf" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.dec" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.dsc" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.fdf" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.robot" . python-mode))

(add-to-list 'auto-mode-alist '("\\.bin\\'" . hexl-mode))
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
      browse-url-generic-program "C:/Program Files (x86)/Google/Chrome/Application/chrome.exe")
)
(when (eq system-type 'gnu/linux)
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "firefox")
)

(global-set-key (kbd "C-x C-b") 'ibuffer)

(add-hook 'ibuffer-mode-hook
	  #'(lambda ()
	     (ibuffer-auto-mode 1)))


(use-package async)

(when (eq system-type 'windows-nt)
  (setq org-agenda-files '("H:/zthomas/private/org"))
  )
(when (eq system-type 'gnu/linux)
  (setq org-agenda-files '("/mnt/nas/org"))
  )

(setq ibuffer-saved-filter-groups
          (quote (("default"
                   ("dired" (mode . dired-mode))
                   ("emacs" (or
                             (name . "^\\*scratch\\*$")
                             (name . "^\\*Messages\\*$")))
		   ("pdfs" (name . "\\.pdf"))
                   ))))

(require 'ibuf-ext)
(add-to-list 'ibuffer-never-show-predicates "\\.org")

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

(use-package cuda-mode
  :mode "\\.cu\\'")

(use-package olivetti
  :hook ((text-mode         . olivetti-mode)
         (prog-mode         . olivetti-mode)
         (Info-mode         . olivetti-mode)
         (org-mode          . olivetti-mode)
         (nov-mode          . olivetti-mode)
         (markdown-mode     . olivetti-mode)
         (mu4e-view-mode    . olivetti-mode)
         (elfeed-show-mode  . olivetti-mode)
         (mu4e-compose-mode . olivetti-mode))
 :custom
  (olivetti-body-width 120)
  :delight " ⊗") ; Ⓐ ⊛

(winner-mode)

(use-package sticky-shell
  :ensure t ; install
  :custom
  (sticky-shell-global-mode)
  ;; add your customization here
  )

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
  :ensure t
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))
