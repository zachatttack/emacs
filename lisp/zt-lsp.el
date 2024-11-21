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
         (python-mode . lsp)
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


(use-package dap-mode
  :config
  (dap-mode 1)
  ;; (dap-ui-mode 1)
  ;; (dap-tooltip-mode 1)
  ;; (tooltip-mode 1)
  ;; (dap-ui-controls-mode 1)
  )

(require 'dap-gdb)
(setq dap-debug-template-configurations
      '(
        ("Zephyr"
         :type "gdbserver"
         :request "launch"
         :gdbpath "/home/zach/zephyr-sdk-0.16.5-1/arm-zephyr-eabi/bin/arm-zephyr-eabi-gdb" ;; Specify the path to your custom GDB executable
         :target ":2331"
         :executable "${workspaceFolder}/build/zephyr/zephyr.elf"
         :cwd "${workspaceFolder}"
         :environment []
         :args []
         :remote :json-true
         :stopAtEntry t
         :externalConsole nil)))


(provide 'zt-lsp)
