(define-package "piglet-emacs" "0.0.1"
  "Major mode and editor integration for the Piglet programming language."
  '((emacs "29.0")
    ;; piglet-mode
    (rainbow-delimiters "2.1.5")
    (aggressive-indent "1.10.0")
    (smartparens "1.11.0")
    ;; pdp
    (websocket "1.14")
    (cbor "20230429"))
  :homepage "https://github.com/piglet-lang"
  :keywords '("piglet" "languages" "lisp" "tree-sitter"))
