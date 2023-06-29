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

(defcustom pdp-result-buffer-name "*piglet-result*"
  "Buffer name to use when showing evaluation results in a separate buffer."
  :type 'string
  :safe #'stringp
  :group 'piglet)

(defcustom pdp-result-destination 'minibuffer
  "Where should, by default, evaluation results be shown"
  :type '(choice (const :tag "Minibuffer" minibuffer)
                 (const :tag "Result buffer" result-buffer)
                 (const :tag "REPL buffer" repl-buffer))
  :safe #'symbolp
  :group 'piglet)

(defcustom pdp-pretty-print-result-p nil
  "Should evaluation results by default be pretty printed?"
  :type 'boolean
  :safe #'booleanp
  :group 'piglet)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internals

(defvar pdp--connections
  ()
  "List of active PDP connections.")

(defvar pdp--server
  nil
  "PDP websocket server, see `pdp-start-server!'")

(defvar pdp--message-counter
  0
  "Incrementing value used to match replies to handlers.")

(defvar pdp--handlers
  ()
  "Association list from message number to handler function.")

;; To debug issues set this variable
;; (setq websocket-callback-debug-on-error t)

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
    (when handler
      (funcall handler msg))))

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
  ;; (message (apply 'concat (mapcar (lambda (x) (format "%02x" x))
  ;;                                 (cbor<-elisp msg))))
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

(defun pdp--eval-minibuffer-handler (result)
  (message "=> %s" result))

(defun pdp--eval-insert-handler (result)
  (when (not (and (bolp) (eolp)))
    (end-of-line)
    (insert "\n"))
  (insert "=> ")
  (insert result))

(defun pdp--eval-to-buffer-handler (result)
  (with-current-buffer (get-buffer-create pdp-result-buffer-name)
    (erase-buffer)
    (insert result)
    (goto-char (point-min))
    (display-buffer (current-buffer))
    (piglet-mode)))

(defun pdp--get-eval-handler (opts)
  (let* ((destination (if-let ((dest (assoc 'destination opts)))
                          (cdr dest)
                        pdp-result-destination))
         (pretty-print (if-let ((pp (assoc 'pretty-print opts)))
                           (cdr pp)
                         pdp-pretty-print-result-p))
         (format-handler (if pretty-print
                             (lambda (msg) (message "Not implemented"))
                           (lambda (msg) (pdp--msg-get msg "result"))))
         (insert-handler (cl-case destination
                           (minibuffer #'pdp--eval-minibuffer-handler)
                           (result-buffer #'pdp--eval-to-buffer-handler)
                           (insert #'pdp--eval-insert-handler)
                           (repl (lambda (_) (message "Not implemented"))))))
    (lambda (msg)
      (funcall
       insert-handler
       (funcall format-handler msg)))))

(defun pdp-op-eval (code-str start line opts)
  (set-text-properties 0 (length code-str) nil code-str)
  (let ((msg (pdp-msg
              `(("op" . "eval")
                ("code" . ,code-str)
                ("line" . ,line)
                ("start" . ,start)))))
    (pdp-send (pdp-add-handler msg (pdp--get-eval-handler opts)))))

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

;; When used with a browser it's best to enable `url-handler-mode'
(defun pdp-jump-to-definition ()
  (interactive)
  (let ((node (treesit-node-at (point))))
    (when (equal "symbol" (treesit-node-type node))
      (pdp-op-resolve-meta
       (treesit-node-text node)
       (lambda (reply)
         (xref-push-marker-stack)
         ;; (message "RESULT %s" reply)
         (with-temp-buffer
           (insert (pdp--msg-get reply "result"))
           (treesit-parser-create 'piglet)
           (let ((file (treesit-node-text
                        (cdr (assoc 'val (treesit-query-capture 'piglet pdp--file-query)))))
                 (char (treesit-node-text
                        (cdr (assoc 'val (treesit-query-capture 'piglet pdp--start-query))))))
             (with-current-buffer (find-file (json-parse-string file))
               (goto-char (string-to-number char))))))))))

(defun pdp-eval-node (node opts)
  (pdp-op-eval
   (treesit-node-text node)
   (treesit-node-start node)
   (line-number-at-pos (treesit-node-start node))
   opts))

(defun pdp--eval-prefix-to-opts (prefix-arg)
  (cl-case prefix-arg
    (1 '((destination . insert)))
    (2 '((destination . result-buffer)))
    (t '())))

(defun pdp--eval (opts)
  (cl-case (cdr (assoc 'form opts))
    (last-sexp (let* ((start (scan-sexps (point) -1))
                      (node (treesit-node-at start)))
                 (pdp-eval-node
                  (if (treesit-node-check node 'named)
                      node
                    (treesit-node-parent node))
                  opts)))

    (outer-sexp (pdp-eval-node
                 (alist-get
                  'expr
                  (or (treesit-query-capture 'piglet
                                             '((list) @expr) (point) (+ (point) 1))
                      (treesit-query-capture 'piglet
                                             '((dict) @expr) (point) (+ (point) 1))))
                 opts))

    (buffer (pdp-op-eval
             (buffer-substring (point-min) (point-max)) 0 0
             opts))

    (region (pdp-op-eval
             (buffer-substring (mark) (point))
             (mark)
             (line-number-at-pos (mark))
             opts))))

(defun pdp-eval-last-sexp (prefix)
  "Evaluate the last sexp at point. If PREFIX exists or is 1 then insert
   the result into the current buffer. If PREFIX is 2 then insert the result
   into a *pdp-result* buffer."
  (interactive "P")
  (pdp--eval (cons '(form . last-sexp) (pdp--eval-prefix-to-opts prefix))))

(defun pdp-eval-outer-sexp (prefix)
  "Evaluate the outermost sexp at point. If PREFIX exists or is 1 then insert
   the result into the current buffer. If PREFIX is 2 then insert the result
   into a *pdp-result* buffer."
  (interactive "P")
  (pdp--eval (cons '(form . outer-sexp) (pdp--eval-prefix-to-opts prefix))))

(defun pdp-eval-buffer (prefix)
  "Evaluate the entire buffer"
  (interactive "P")
  (pdp--eval (cons '(form . buffer) (pdp--eval-prefix-to-opts prefix))))

(defun pdp-eval-region (prefix)
  "Evaluate the currently selected region."
  (interactive "P")
  (pdp--eval (cons '(form . region) (pdp--eval-prefix-to-opts prefix))))

(defun pdp-toggle-result-destination ()
  "Change the default location where evalation results are shown."
  (interactive)
  (let ((next (cl-case pdp-result-destination
                (minibuffer 'result-buffer)
                (result-buffer 'repl-buffer)
                (repl-buffer 'minibuffer))))
    (customize-save-variable 'pdp-result-destination next)
    (message (concat "[PDP] Evaluation results will go to "
                     (cl-case next
                       (minibuffer "the minibuffer")
                       (repl-buffer "the piglet REPL buffer")
                       (result-buffer "a separate result buffer"))))))

(defun pdp-toggle-pretty-printing ()
  "Toggle pretty printing of results on/off"
  (interactive)
  (let ((next (not pdp-pretty-print-result-p)))
    (customize-save-variable 'pdp-pretty-print-result-p next)
    (message (concat "[PDP] Pretty-printing has been turned "
                     (if next "on." "off.")))))

;; Generate all permutations, for direct access that overrides the toggles
(dolist (form '((last-sexp . "the last expression before point")
                (outer-sexp . "the top-level expression at point")
                (buffer . "the entire buffer")
                (region . "the currently selected region")))
  (dolist (destination '((minibuffer . "in the minibuffer")
                         (insert . "inserted into the current buffer")
                         (result-buffer . "in a separate result buffer")
                         (repl . "inserted into the REPL buffer")))
    (dolist (pretty-print '(nil t))
      (defalias (intern (concat "pdp-eval-"
                                (symbol-name (car form))
                                "-to-"
                                (symbol-name (car destination))
                                (if pretty-print
                                    "-pretty-print"
                                  "")))
        `(lambda ()
           ,(concat "Evaluate " (cdr form) " and show the result\n" (cdr destination)
                    (if pretty-print
                        ", pretty-printed."
                      "."))
           (interactive)
           (pdp--eval '((form . ,(car form))
                        (destination . ,(car destination))
                        (pretty-print . ,pretty-print))))))))

(provide 'pdp)


;; (pdp-stop-server!)
;; (pdp-start-server!)

;; (let ((msg
;;        '(("op" . "eval") ("code" . "1234") ("line" . 27) ("start" . 796) ("location" . "/home/arne/Piglet/piglet-lang/packages/piglet/src/pdp-client.pig") ("module" . #("pdp-client" 0 10 (face default fontified t))) ("package" . "https://piglet-lang.org/packages/piglet") ("reply-to" . 1385))
;;        ))
;;   (message (apply 'concat (mapcar (lambda (x) (format "%02x" x))
;;                                   (cbor<-elisp msg)))))

;; (pdp-send '(("op" . "eval") ("code" . "1235") ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; pdp.el ends here
