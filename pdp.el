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
(require 'xref)

(setq pdp--connections ())
(setq pdp--server nil)
(setq pdp--message-counter 0)
(setq pdp--handlers ())

(defun pdp--msg-get (msg k)
  (alist-get k msg nil nil #'equal))

(defun pdp--on-open (ws)
  (add-to-list #'pdp--connections ws)
  (message "[Piglet] PDP conn opened, %d active connections" (length pdp--connections)))

(defun pdp--on-message (ws frame)
  (let* ((msg (cbor->elisp (websocket-frame-payload frame)))
         (op (pdp--msg-get msg "op"))
         (to (pdp--msg-get msg "to"))
         (handler (and to (alist-get to pdp--handlers))))
    (if handler
        (funcall handler msg)
      (message "=> %s" (pdp--msg-get msg "result")))))

(defun pdp--on-close (ws)
  (setq pdp--connections
        (seq-remove (lambda (conn) (eq ws conn))
                    pdp--connections))
  (message "[Piglet] PDP conn closed, %d active connections" (length pdp--connections)))

(defun pdp-start-server! ()
  (interactive)
  (if (not pdp--server)
      (progn
        (setq pdp--server
              (websocket-server
               17017
               :host 'local
               :on-open #'pdp--on-open
               :on-message #'pdp--on-message
               :on-close #'pdp--on-close))
        (message "[Piglet] PDP server started on port: 17017"))
    (message "[Piglet] PDP server already running.")))

(defun pdp-stop-server! (prefix)
  "Stop the PDP server. With PREFIX, force closes the zombie server on 17017.

   Force closing the server may be needed for default webscoket server when the
   pointer to the existing connection is lost, mostly only useful during
   development or if it got in a bad state somehow."
  (interactive "P")
  (when pdp--server
    (websocket-server-close pdp--server))
  (setq pdp--server nil)
  (setq pdp--connections nil)
  (when prefix
    (delete-process "websocket server on port 17017"))
  (message "[Piglet] PDP server stopped."))

(defun pdp-msg (kvs)
  (append
   kvs
   `(("location" . ,buffer-file-name)
     ("module" . ,(piglet--module-name))
     ("package" . ,piglet-package-name))))

(defun pdp-add-handler (msg handler)
  (setq pdp--message-counter (+ pdp--message-counter 1))
  (setq pdp--handlers (cons (cons pdp--message-counter handler)
                            pdp--handlers))
  (append msg
          `(("reply-to" . ,pdp--message-counter))))

(defun pdp-send (msg)
  ;; (message "[PDP] -> %S" msg)
  (let ((payload (cbor<-elisp msg)))
    (seq-do (lambda (client)
              (when (websocket-openp client)
                (websocket-send
                 client
                 (make-websocket-frame
                  :opcode 'binary
                  :payload payload
                  :completep t))))
            pdp--connections)))

(defun pdp-op-eval (code-str start line insert)
  (set-text-properties 0 (length code-str) nil code-str)
  (let ((msg (pdp-msg
              `(("op" . "eval")
                ("code" . ,code-str)
                ("line" . ,line)
                ("start" . ,start)))))
    (pdp-send
     (if insert
         (pdp-add-handler
          msg
          (lambda (msg)
            (when (not (and (bolp) (eolp)))
              (end-of-line)
              (insert "\n"))
            (insert "=> ")
            (insert (pdp--msg-get msg "result"))))
       msg))))

(setq pdp--start-query
      (treesit-query-compile
       'piglet
       "(source . (dict (keyword) @kw . (number) @val) (#equal @kw \":start\"))"))

(setq pdp--file-query
      (treesit-query-compile
       'piglet
       "(source . (dict (keyword) @kw . (string) @val) (#equal @kw \":file\"))"))

(defun pdp-op-resolve-meta (var-sym handler)
  (pdp-send
   (pdp-add-handler
    (pdp-msg
     `(("op" . "resolve-meta")
       ("var" . ,var-sym)))
    handler)))

(defun pdp-jump-to-definition ()
  (interactive)
  (let ((node (treesit-node-at (point))))
    (when (equal "symbol" (treesit-node-type node))
      (pdp-op-resolve-meta
       (treesit-node-text node)
       (lambda (reply)
         (xref-push-marker-stack)
         (with-temp-buffer
           (insert (pdp--msg-get reply "result"))
           (treesit-parser-create 'piglet)
           (let ((file (treesit-node-text
                        (cdr (assoc 'val (treesit-query-capture 'piglet pdp--file-query)))))
                 (char (treesit-node-text
                        (cdr (assoc 'val (treesit-query-capture 'piglet pdp--start-query))))))
             (with-current-buffer (find-file (json-parse-string file))
               (goto-char (string-to-number char))))))))))

(defun pdp-eval-node (node insert)
  (pdp-op-eval
   (treesit-node-text node)
   (treesit-node-start node)
   (line-number-at-pos (treesit-node-start node))
   insert))

(defun pdp-eval-outer-sexp (prefix)
  (interactive "P")
  (pdp-eval-node
   (alist-get
    'expr
    (treesit-query-capture 'piglet
                           '((list) @expr) (point) (+ (point) 1)))
   prefix))

(defun pdp-eval-last-sexp (prefix)
  (interactive "P")
  (let* ((start (scan-sexps (point) -1))
         (node (treesit-node-at start)))
    (pdp-eval-node
     (if (treesit-node-check node 'named)
         node
       (treesit-node-parent node))
     prefix)))

(defun pdp-eval-buffer ()
  (interactive)
  (pdp-op-eval
   (buffer-substring (point-min) (point-max)) 0 0
   nil))

(defun pdp-eval-region ()
  (interactive)
  (pdp-op-eval
   (buffer-substring (mark) (point))
   (mark)
   (line-number-at-pos (mark))
   nil))

(provide 'pdp)


;; (pdp-stop-server!)
;; (pdp-start-server!)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; pdp.el ends here
