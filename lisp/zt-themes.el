;; (use-package kaolin-themes
;;  :config
;;   (load-theme 'kaolin-temple t)
;;  (kaolin-treemacs-theme))

(use-package catppuccin-theme
  :config
  (setq catppuccin-flavor 'macchiato)
  )

;; (use-package spaceway-theme
;;   :ensure nil
;;   :load-path "lisp/spaceway/"
;;   :config
;;   (global-hl-line-mode t)
;;   (set-frame-parameter nil 'cursor-color "#dc322f")
;;   (add-to-list 'default-frame-alist '(cursor-color . "#dc322f"))

;; (when my/my-system
;;   (set-frame-parameter nil 'alpha-background 85)
;;   (add-to-list 'default-frame-alist '(alpha-background . 85)))

;; (load-theme 'spaceway t)
;; (setenv "SCHEME" "dark")
;; )
(use-package spacemacs-theme
  :defer t
  ;; (load-theme 'spacemacs-dark t)
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

(use-package all-the-icons)

(use-package all-the-icons-completion
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))



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

(use-package modus-themes
  :init
  (setq modus-themes-italic-constructs t
      modus-themes-bold-constructs nil)
  ;; (setq modus-themes-mode-line '(borderless))
  )

(defun zt/setup-appearance (frame)
  (with-selected-frame frame
    (remove-hook 'after-make-frame-functions 'zt/setup-appearance)
    ;; (catppuccin-reload)
    (load-theme 'modus-vivendi t)
    ))

(if (daemonp)
    (add-hook 'after-make-frame-functions 'zt/setup-appearance)
  (zt/setup-appearance (car (frame-list)))
  )



(provide 'zt-themes)
