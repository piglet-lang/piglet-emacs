;;; pdp.el --- Piglet Dev Protocol, interactive programming over websocket -*- lexical-binding: t -*-

;; Author: Arne Brasseur <arne@arnebrasseur.net>
;; Filename: pdp.el
;; Package-Requires: ((websocket) (cbor))
;; Keywords: piglet languages repl

;;; Commentary:

;;; Code:

(require 'websocket)
(require 'cbor)
(require 'seq)

(setq pdp--connections ())
(setq pdp--server nil)

(defun pdp--on-open (ws)
  (message "PDP conn Opened %S" ws)
  (add-to-list #'pdp--connections ws))

(defun pdp--on-message (ws frame)
  (let* ((msg (cbor->elisp (websocket-frame-payload frame)))
         (op (alist-get "op" msg nil nil #'equal)))
    (when (equal op "eval")
      (message "=> %s" (alist-get "result" msg nil nil #'equal)))))

(defun pdp--on-close (ws)
  (message "PDP conn closed %S" ws)
  (setq pdp--connections
        (seq-remove (lambda (conn) (eq ws conn))
                    pdp--connections)))

(defun pdp-start-server! ()
  (interactive)
  (when (not pdp--server)
    (setq pdp--server
          (websocket-server
           17017
           :host 'local
           :on-open #'pdp--on-open
           :on-message #'pdp--on-message
           :on-close #'pdp--on-close))))

(defun pdp-stop-server! ()
  (interactive)
  (when pdp--server
    (websocket-server-close pdp--server))
  (setq pdp--server nil)
  (setq pdp--connections nil))

(defun pdp-send (value)
  (seq-do (lambda (client)
            (if (websocket-openp client)
                (websocket-send
                 client
                 (make-websocket-frame
                  :opcode 'binary
                  :payload (cbor<-elisp value)
                  :completep t))))
          pdp--connections))

(defun pdp-op-eval (code-str)
  (set-text-properties 0 (length code-str) nil code-str)
  (pdp-send
   `(("op" . "eval")
     ("code" . ,code-str)
     ("location" . ,buffer-file-name)
     ("package" . ,piglet-package-name))))

(defun pdp-eval-outer-sexp ()
  (interactive)
  (pdp-op-eval
   (treesit-node-text
    (alist-get
     'expr
     (treesit-query-capture 'piglet
                            '((list) @expr) (point) (+ (point) 1))))))

(defun pdp-eval-last-sexp ()
  (interactive)
  (let* ((start (scan-sexps (point) -1))
         (node (treesit-node-at start)))
    (pdp-op-eval
     (treesit-node-text
      (if (treesit-node-check node 'named)
          node
        (treesit-node-parent node))))))

(defun pdp-eval-buffer ()
  (interactive)
  (pdp-op-eval
   (buffer-substring (point-min) (point-max))))

(defun pdp-eval-region ()
  (interactive)
  (pdp-op-eval
   (buffer-substring (mark) (point))))

(provide 'pdp)


;; (pdp-stop-server!)
;; (pdp-start-server!)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; pdp.el ends here
