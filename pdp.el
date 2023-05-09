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
(alist-get "op"
           '
           (("op" . "eval") ("result" . "3"))
           nil nil #'equal)
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

;; (lambda (ws frame)
;;   (websocket-send-text
;;    ws (websocket-frame-text frame)))

(defun pdp-start-server! ()
  (when (not pdp--server)
    (setq pdp--server
          (websocket-server
           17017
           :host 'local
           :on-open #'pdp--on-open
           :on-message #'pdp--on-message
           :on-close #'pdp--on-close))))

(defun pdp-stop-server! ()
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
  (pdp-send
   `(("op" . "eval")
     ("code" . ,code-str))))

(defun pdp-eval-outer-sexp ()
  (interactive)
  (pdp-op-eval
   (treesit-node-text
    (alist-get
     'expr
     (treesit-query-capture 'piglet
                            '((list) @expr) (point) (+ (point) 1))))))


(provide 'pdp)


;; (pdp-stop-server!)
;; (pdp-start-server!)
;; pdp--server
;; (seq-remove
;;  (lambda (conn)
;;    (eq conn
;;        (car pdp--connections)))
;;  pdp--connections)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; pdp.el ends here
