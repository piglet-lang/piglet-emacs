;;; piglet-mode.el --- Major mode for Piglet LISP -*- lexical-binding: t -*-

;; Author: Arne Brasseur <arne@arnebrasseur.net>
;; Filename: piglet-mode.el
;; Package-Requires: ((rainbow-delimiters) (aggressive-indent))
;; Keywords: piglet languages tree-sitter

;;; Commentary:

;; Major mode and interactive development commands for Piglet. Based on
;; Tree-sitter, so you will need Emacs 29. Work in progress.

;;; Code:

(require 'treesit)
(require 'rainbow-delimiters)
(require 'aggressive-indent)

;;(setq treesit-language-source-alist nil)
(add-to-list
 'treesit-language-source-alist
 '(piglet ;;"https://github.com/piglet-lang/tree-sitter-piglet.git"
   "/home/arne/github/tree-sitter-piglet"
   ))

;; (treesit-install-language-grammar 'piglet)
(defvar piglet-mode-indent-offset
  2)

(defvar piglet-ts--indent-rules
  '((piglet
     ((parent-is "list") first-sibling 2)
     ((parent-is "vector") first-sibling 1)
     )))

(setq piglet-mode--font-lock-settings
      (treesit-font-lock-rules
       :feature 'brackets
       :language 'piglet
       '((_ (_ (_ (_ (_ (_ (_ (_ ["(" ")" "[" "]"] @rainbow-delimiters-depth-7-face)))))))))

       :feature 'brackets
       :language 'piglet
       '((_ (_ (_ (_ (_ (_ (_ ["(" ")" "[" "]"] @rainbow-delimiters-depth-6-face))))))))

       :feature 'brackets
       :language 'piglet
       '((_ (_ (_ (_ (_ (_ ["(" ")" "[" "]"] @rainbow-delimiters-depth-5-face)))))))

       :feature 'brackets
       :language 'piglet
       '((_ (_ (_ (_ (_ ["(" ")" "[" "]"] @rainbow-delimiters-depth-4-face))))))

       :feature 'brackets
       :language 'piglet
       '((_ (_ (_ (_ ["(" ")" "[" "]"] @rainbow-delimiters-depth-3-face)))))

       :feature 'brackets
       :language 'piglet
       '((_ (_ (_ ["(" ")" "[" "]"] @rainbow-delimiters-depth-2-face))))

       :feature 'brackets
       :language 'piglet
       '((_ (_ ["(" ")" "[" "]"] @rainbow-delimiters-depth-1-face)))

       :feature 'keywords
       :language 'piglet
       '((symbol1) @font-lock-keyword-face)))

(treesit-query-validate 'piglet '(([(list) (vector)] ["(" ")" "[" "]"]) @rainbow-delimiters-depth-2-face))

(setq piglet-mode--font-features '((brackets keywords)))

(defun piglet-setup-font-lock ()
  (interactive)
  (setq-local treesit-font-lock-settings piglet-mode--font-lock-settings)
  (setq-local treesit-font-lock-feature-list piglet-mode--font-features)
  (treesit-major-mode-setup))

(define-derived-mode piglet-mode prog-mode "Piglet"
  "Major mode for editing Piglet files."
  (unless (treesit-ready-p 'piglet)
    (if (yes-or-no-p "Tree-sitter grammar for Piglet not found. Install it now? ")
        (treesit-install-language-grammar 'piglet)
      (error "Tree-sitter for Piglet isn't available")))

  (aggressive-indent-mode)

  (treesit-parser-create 'piglet)
  ;; Font-lock.
  (setq-local treesit-font-lock-feature-list piglet-mode--font-features)
  (setq-local treesit-font-lock-settings piglet-mode--font-lock-settings)

  ;; Comments
  (setq-local comment-start ";; ")
  (setq-local comment-start-skip ";+ *")
  (setq-local comment-end "")

  ;; Electric
  (setq-local electric-indent-chars
              (append "[]{}()" electric-indent-chars))

  ;; Indent
  (setq-local treesit-simple-indent-rules piglet-ts--indent-rules)

  ;; Navigation
  ;; (setq-local treesit-defun-type-regexp ...)
  ;; (setq-local treesit-defun-name-function #'piglet-mode--defun-name)

  ;; Imenu.
  ;; (setq-local treesit-simple-imenu-settings '())

  (treesit-major-mode-setup))

(add-to-list 'auto-mode-alist '("\\.pig\\'" . piglet-mode))

(provide 'piglet-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; piglet-mode.el ends here
