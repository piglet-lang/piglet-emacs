;;; piglet-mode.el --- Major mode for Piglet LISP -*- lexical-binding: t -*-

;; Author: Arne Brasseur <arne@arnebrasseur.net>
;; Filename: piglet-mode.el

;;; Commentary:

;; Major mode and interactive development commands for Piglet. Based on
;; Tree-sitter, so you will need Emacs 29. Work in progress.

;;; Code:

(require 'treesit)
(require 'rainbow-delimiters)
(require 'aggressive-indent)
(require 'smartparens)
(require 'filenotify)

(when (not (fboundp 'treesit-language-available-p))
  (read-key "WARNING: Emacs was not compiled with tree-sitter support (--with-tree-sitter), make sure libtree-sitter0 is installed before building."))

(add-to-list
 'treesit-language-source-alist
 '(piglet "https://github.com/piglet-lang/tree-sitter-piglet.git"))

(defvar piglet-ts--indent-rules
  '((piglet
     ((parent-is "source") first-sibling 0)
     ((parent-is "list") first-sibling 2)
     ((parent-is "vector") first-sibling 1)
     ((parent-is "dict") first-sibling 1))))

(defvar piglet-package-names
  nil
  "Association list from package.pig location to :pkg:name")

(setq piglet-mode--font-lock-settings
      (treesit-font-lock-rules
       :feature 'brackets
       :language 'piglet
       '((_ (_ (_ (_ (_ (_ (_ (_ ["(" ")" "[" "]" "{" "}"] @rainbow-delimiters-depth-8-face)))))))))

       :feature 'brackets
       :language 'piglet
       '((_ (_ (_ (_ (_ (_ (_ ["(" ")" "[" "]" "{" "}"] @rainbow-delimiters-depth-7-face))))))))

       :feature 'brackets
       :language 'piglet
       '((_ (_ (_ (_ (_ (_ ["(" ")" "[" "]" "{" "}"] @rainbow-delimiters-depth-6-face)))))))

       :feature 'brackets
       :language 'piglet
       '((_ (_ (_ (_ (_ ["(" ")" "[" "]" "{" "}"] @rainbow-delimiters-depth-5-face))))))

       :feature 'brackets
       :language 'piglet
       '((_ (_ (_ (_ ["(" ")" "[" "]" "{" "}"] @rainbow-delimiters-depth-4-face)))))

       :feature 'brackets
       :language 'piglet
       '((_ (_ (_ ["(" ")" "[" "]" "{" "}"] @rainbow-delimiters-depth-3-face))))

       :feature 'brackets
       :language 'piglet
       '((_ (_ ["(" ")" "[" "]" "{" "}"] @rainbow-delimiters-depth-2-face)))

       :feature 'identifiers
       :language 'piglet
       '((list :anchor (symbol) @font-lock-function-call-face))

       :feature 'identifiers
       :language 'piglet
       '((symbol) @default)


       :feature 'identifiers
       :language 'piglet
       '((keyword) @font-lock-constant-face)

       :feature 'identifiers
       :language 'piglet
       '((prefix_name) @font-lock-constant-face)

       ;; :feature 'built-ins
       ;; :language 'piglet
       ;; '((symbol) @font-lock-keyword-face
       ;;   (:equal @font-lock-keyword-face "defn"))

       ;; :feature 'identifiers
       ;; :language 'piglet
       ;; '((qname) @font-lock-constant-face)

       :feature 'numbers
       :language 'piglet
       '((number) @font-lock-number-face)

       :feature 'strings
       :language 'piglet
       '((string) @font-lock-string-face)

       :feature 'comments
       :language 'piglet
       '((comment) @font-lock-comment-face)

       ))

;; (treesit-query-capture 'piglet
;;                        '((symbol) @font-lock-keyword-face
;;                          (:equal @font-lock-keyword-face "defn"))
;;                        )

(setq piglet-mode--font-features '((brackets identifiers strings numbers built-ins comments)))

(defun piglet-setup-font-lock ()
  (interactive)
  (setq-local treesit-font-lock-settings piglet-mode--font-lock-settings)
  (setq-local treesit-font-lock-feature-list piglet-mode--font-features)
  (treesit-major-mode-setup))

(sp-local-pair 'piglet-mode "'" nil :actions nil)
(sp-local-pair 'piglet-mode "`" nil :actions nil)

(setq piglet--find-package-name-query
      (treesit-query-compile
       'piglet
       "(source
          (dict
            (prefix_name) @prefix . (symbol) @sym)
            (#equal @prefix \":pkg:name\"))"))

(setq piglet--find-module-name-query
      (treesit-query-compile
       'piglet
       "(source .
          (list
            (symbol) @list-head . (symbol) @name)
            (#equal @list-head \"module\"))"))

(defun piglet--package-root (file)
  (locate-dominating-file file "package.pig"))

(defun piglet--package-name* (pkg-root)
  (if (not pkg-root)
      (concat "file://" (directory-file-name (file-name-directory file)))
    (let ((pkg-pig (expand-file-name "package.pig" pkg-root)))
      (with-temp-buffer
        (insert-file-contents pkg-pig)
        (treesit-parser-create 'piglet)
        (let ((node (cdr (assoc 'sym (treesit-query-capture 'piglet piglet--find-package-name-query)))))
          (if node (treesit-node-text node) (concat "file://" (directory-file-name (expand-file-name pkg-root)))))))))

(defun piglet--package-name (pkg-root)
  "Return the package for the given package root directory, gets memoized,
but also updated when package.pig changes."
  (when pkg-root
    (or (cdr (assoc pkg-root piglet-package-names))
        (let* ((pkg-name (piglet--package-name* pkg-root)))
          (setq piglet-package-names (cons (cons pkg-root pkg-name)
                                           piglet-package-names))
          (file-notify-add-watch
           (expand-file-name "package.pig" pkg-root)
           '(change)
           (lambda (event)
             (setq piglet-package-names (assq-delete-all pkg-root piglet-package-names))
             (piglet--package-name pkg-root)))
          pkg-name))))

(defun piglet-package-name (file)
  "Return the package name for the given piglet file"
  (piglet--package-name (piglet--package-root file)))

(defun piglet--module-name ()
  (treesit-node-text
   (cdr (assoc 'name (treesit-query-capture 'piglet piglet--find-module-name-query)))))

(defun piglet-mode--is-def-sym-p (node)
  (and (equal "symbol" (treesit-node-type node))
       (equal "list" (treesit-node-type (treesit-node-parent node)))
       (member (treesit-node-text (treesit-node-child (treesit-node-parent node) 1))
               '("def" "defn" "defmacro"))
       (equal node (treesit-node-child (treesit-node-parent node) 2))))

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

  ;; Imenu
  (setq-local treesit-defun-name-function #'treesit-node-text)
  (setq-local treesit-simple-imenu-settings
              '((nil "\\`symbol\\'" piglet-mode--is-def-sym-p nil)))

  ;; Navigation
  ;; (setq-local treesit-defun-type-regexp ...)
  ;; (setq-local treesit-defun-name-function #'piglet-mode--defun-name)

  ;; Imenu.
  ;; (setq-local treesit-simple-imenu-settings '())

  (treesit-major-mode-setup))


(add-to-list 'auto-mode-alist '("\\.pig\\'" . piglet-mode)) ;; currently in use
(add-to-list 'auto-mode-alist '("\\.pigl\\'" . piglet-mode)) ;; we're planning to use this instead

(provide 'piglet-mode)

(when nil

  (treesit--install-language-grammar-1 nil 'piglet "https://github.com/piglet-lang/tree-sitter-piglet")
  (module-load (expand-file-name "tree-sitter/libtree-sitter-piglet.so" user-emacs-directory))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; piglet-mode.el ends here
