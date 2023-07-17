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


(set-face-attribute 'default nil :font "Fira Code" :height 140)

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

(use-package kaolin-themes
 :config
  (load-theme 'kaolin-temple t)
 (kaolin-treemacs-theme))

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
  (general-define-key "M-n" 'scroll-up)
  (general-define-key "M-e" 'scroll-down)
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
  "wl" '(zt-toggle-window-dedication :which-key "Toggle window dedication")
  "gt" 'eyebrowse-next-window-config
  "gT" 'eyebrowse-prev-window-config
  "gc" 'eyebrowse-close-window-config
  "g." 'eyebrowse-switch-to-window-config
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
    (when (file-directory-p "~/")
      (setq projectile-project-search-path '("~/")))
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
  )

(use-package dired-single)

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
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

;; (autoload 'turn-on-ctags-auto-update-mode "ctags-update" "turn on 'ctags-auto-update-mode'." t)
;; (add-hook 'c-mode-common-hook  'turn-on-ctags-auto-update-mode)

(use-package tree-sitter)
(use-package tree-sitter-langs)
(tree-sitter-require 'c)
(global-tree-sitter-mode)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

;; (use-package treesit-auto
;;   :config (setq treesit-auto-install 'prompt)
;;   :config
;;   (global-treesit-auto-mode))

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
		 (setq org-directory "/mnt/home/zthomas/private/org"))
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

(when (window-system)
  (set-frame-font "Fira Code"))
(add-to-list 'default-frame-alist '(font . "Fira Code"))
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
  ;; (global-idle-highlight-mode)
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
      browse-url-generic-program "firefox")
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

(setq ibuffer-saved-filter-groups
          (quote (("default"
                   ("shells" (mode . shell-mode))
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

;; (use-package olivetti
;;   :custom
;;   (olivetti-body-width 130))
;; (use-package auto-olivetti
;;   :elpaca (auto-olivetti :host sourcehut :repo "ashton314/auto-olivetti")
;;   :config
;;   (auto-olivetti-mode))

(winner-mode)

(use-package sticky-shell
  :config
  (sticky-shell-global-mode)
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
  :custom
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))


(use-package corfu
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
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  ;;(add-to-list 'completion-at-point-functions #'cape-history)
  ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-symbol)
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

(modify-syntax-entry ?_ "w")

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

;; (use-package eyebrowse
;;   :init
;;   (eyebrowse-mode)
;;   )

;; (use-package undo-tree
;;   :ensure t
;;   :init
;;   (global-undo-tree-mode))

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
(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-colemak-dh)
  (meow-motion-overwrite-define-key
   ;; Use e to move up, n to move down.
   ;; Since special modes usually use n to move down, we only overwrite e here.
   '("e" . meow-prev)
   '("N" . end-of-buffer)
   '("E" . beginning-of-buffer)
   '("<escape>" . ignore)
   )
  (meow-leader-define-key
   '("?" . meow-cheatsheet)
   ;; To execute the originally e in MOTION state, use SPC e.
   '("e" . "H-e")
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument))
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("1" . meow-expand-1)
   '("2" . meow-expand-2)
   '("3" . meow-expand-3)
   '("4" . meow-expand-4)
   '("5" . meow-expand-5)
   '("6" . meow-expand-6)
   '("7" . meow-expand-7)
   '("8" . meow-expand-8)
   '("9" . meow-expand-9)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("/" . meow-visit)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-delete)
   '("e" . meow-prev)
   '("E" . meow-prev-expand)
   '("f" . meow-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("m" . meow-left)
   '("M" . meow-left-expand)
   '("i" . meow-right)
   '("I" . meow-right-expand)
   '("j" . meow-join)
   '("k" . meow-kill)
   '("l" . meow-line)
   '("L" . meow-goto-line)
   '("h" . meow-mark-word)
   '("H" . meow-mark-symbol)
   '("n" . meow-next)
   '("N" . meow-next-expand)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("q" . meow-quit)
   '("r" . meow-replace)
   '("s" . meow-insert)
   '("S" . meow-open-above)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-search)
   '("w" . meow-next-word)
   '("W" . meow-next-symbol)
   '("x" . meow-delete)
   '("X" . meow-backward-delete)
   '("y" . meow-save)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("<escape>" . ignore)))

(use-package meow
  :config
  (meow-setup)
  (meow-global-mode)
  (setq meow-use-clipboard t)
  )

(add-to-list 'meow-mode-state-list '(elfeed-show-mode . motion))

;; (use-package god-mode
;;   :config
;;   (global-set-key (kbd "<escape>") #'god-local-mode)
;;   (define-key god-local-mode-map (kbd "i") 'god-local-mode)
;;   )

(setq tab-bar-close-button-show nil
      tab-bar-new-button-show nil)

(use-package virtualenvwrapper
  :ensure t
  :config
  (venv-initialize-interactive-shells)
  (venv-initialize-eshell)
  )

;; (add-to-list 'tramp-connection-properties
;;              (list (regexp-quote "/sshx:zthomas@zthomas2:")
;;                    "remote-shell" "/usr/bin/zsh"))

;; (use-package forge
;;   :after magit)

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

