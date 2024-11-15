;;; boon-colemak.el --- An Ergonomic Command Mode  -*- lexical-binding: t -*-

;;; Commentary:
;;; Code:
(require 'boon)

;; (define-key boon-select-map "q"  'boon-select-outside-quotes)
;; (define-key boon-select-map "w"  'boon-select-word)
;; (define-key boon-select-map "f"  'boon-select-word) ;; 'rf' is easier to type than 'rw'
;; (define-key boon-select-map "p"  'boon-select-paragraph)
;; (define-key boon-select-map "g"  'boon-select-org-table-cell)

;; (define-key boon-select-map "a"  'boon-select-borders) ;; Around
;; (define-key boon-select-map "r"  'boon-select-justline) ;; Row
;; (define-key boon-select-map "s"  '("symbol" . boon-select-wim)) ;; symbol
;; (define-key boon-select-map "S"  'boon-select-sentence)
;; (define-key boon-select-map "t"  'boon-select-with-spaces)
;; (define-key boon-select-map "T"  'boon-select-org-tree)
;; (define-key boon-select-map "d"  'boon-select-document)

;; (define-key boon-select-map "z"  'boon-select-content) ;; inZide
;; (define-key boon-select-map "x"  'boon-select-outside-pairs) ;; eXpression
;; (define-key boon-select-map "C"  'boon-select-comment)
;; (define-key boon-select-map "c"  'boon-select-inside-pairs) ;; Contents
;; (define-key boon-select-map "b"  'boon-select-block)

;; (define-key boon-select-map "k"  '("blanks" . boon-select-blanks))

;; (define-key boon-moves-map "k" '("bacK to marK" . boon-switch-mark)) ; bacK to marK
;; (define-key boon-moves-map "K" 'xref-pop-marker-stack)

;; (define-key boon-moves-map "j"  '("jump" . xref-find-definitions))
;; (define-key boon-moves-map "J"  'xref-find-references)
;; (define-key boon-moves-map "u"  '("?" . previous-line))
;; (define-key boon-moves-map "y"  '("?" . next-line))
;; (define-key boon-moves-map "U"  'backward-paragraph)
;; (define-key boon-moves-map "Y"  'forward-paragraph)
(define-key boon-moves-map "l"  'occur)
;; (define-key boon-moves-map ";"  '("?" . boon-end-of-line))
(define-key boon-command-map "n"  'boon-take-region)
;; (define-key boon-moves-map "o"  '("?" . boon-smarter-forward))
;; (define-key boon-moves-map "E"  'boon-smarter-upward)
;; (define-key boon-moves-map "I"  'boon-smarter-downward)
;; (define-key boon-moves-map ","  '("?" . boon-beginning-of-expression))
;; (define-key boon-moves-map "."  '("?" . boon-end-of-expression))
;; (define-key boon-moves-map "e"  '("?" . backward-char))
;; (define-key boon-moves-map "i"  '("?" . forward-char))
;; (define-key boon-moves-map "<"  'beginning-of-buffer)
;; (define-key boon-moves-map ">"  'end-of-buffer)
;; (define-key boon-moves-map "m"  'boon-qsearch-previous-at-point)
;; (define-key boon-moves-map "/"  'boon-qsearch-next-at-point)
(define-key boon-moves-map "h"  '("hop" . avy-goto-word-1))
(define-key boon-moves-map "H"  'avy-goto-char)

;; Special keys

;; LEFT HAND

;; Top row
(define-key boon-command-map "q" 'beginning-of-line)
(define-key boon-moves-map "w" 'previous-line)
(define-key boon-moves-map "f" 'next-line)
(define-key boon-command-map "p" 'end-of-line)

(define-key boon-moves-map "W"  'backward-paragraph)
(define-key boon-moves-map "F"  'forward-paragraph)

;; (define-key boon-command-map "P" 'kmacro-end-or-call-macro) ; Play

;; home row
(define-key boon-command-map "a" 'boon-smarter-backward)

(define-key boon-command-map "r" 'backward-char)
(define-key boon-command-map "s" 'forward-char)

(define-key boon-command-map "R" 'boon-smarter-upward)
(define-key boon-command-map "S" 'boon-smarter-downward)

(define-key boon-command-map "t" 'boon-smarter-forward)

(define-key boon-command-map "g" '("goto" . boon-goto-map))

;; d
(define-key boon-command-map "d" 'boon-set-insert-like-state)
(define-key boon-command-map "i" 'boon-set-insert-like-state)

;; Bottom row
;; z
(define-key boon-command-map "z" '("repeat" . boon-repeat-command))
;; x
(define-key boon-command-map "x" 'boon-x-map)
(define-key boon-command-map "X" 'boon-hl-regexp)
;; c
(define-key boon-command-map "c" 'boon-c-god)
(define-key boon-command-map "C"  'boon-exchange)
;; v
(define-key boon-command-map (kbd "C-v") 'boon-open-line-and-insert)
(define-key boon-command-map "V" 'boon-open-next-line-and-insert)
(define-key boon-command-map "v" '("v looks like an insert mark" . boon-set-insert-like-state))

;; b
;; (define-key boon-command-map "B" 'boon-copy-to-register)
;; (define-key boon-command-map "b" '("bank" . insert-register))

;; RIGHT HAND: movement and marking commands.

;; Most of the moves are in boon-moves-map. Yet some moves do not work
;; as selectors, so they are put in the boon-command-map instead.
(define-key boon-command-map (kbd "C-u") 'scroll-down-line)
(define-key boon-command-map (kbd "C-y") 'scroll-up-line)

;; (define-key indent-rigidly-map "i" 'indent-rigidly-right)
;; (define-key indent-rigidly-map "e" 'indent-rigidly-left)

(provide 'boon-colemak-lefty)
;;; boon-colemak.el ends here
