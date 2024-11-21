
(setq org-directory "/home/zach/org")

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-switchb)

(eval-after-load "org"
  '(require 'ox-md nil t))

;; (use-package org-mode
;;   :ensure nil
;;   :config
(setq org-confirm-babel-evaluate nil)
;; )
(use-package org-roam
  :after org
  :init (setq org-roam-v2-ack t) ;; Acknowledge V2 upgrade
  (setq org-roam-dailies-directory "daily/")
  (setq org-roam-dailies-capture-templates
        '(("d" "default" entry "** %?" :if-new
           (file+head+olp "%<%G-W%V>.org" "#+title: %<%G-W%V>\n"
                          ("%<%A %Y-%m-%d>")))))
  :custom
  (org-roam-directory (file-truename (expand-file-name "roam" org-directory)))
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
(setq shr-max-image-proportion 0.8)
(setq org-agenda-span 21)

(setq org-deadline-warning-days 21)






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



(provide 'zt-org)
