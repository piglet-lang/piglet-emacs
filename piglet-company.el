;;; piglet-company.el --- Piglet Dev Protocol, interactive programming over websocket -*- lexical-binding: t -*-

;; Author: Arne Brasseur <arne@arnebrasseur.net>
;; Filename: piglet-company.el
;; Package-Requires: ((company pdp))
;; Keywords: piglet languages completion

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'pdp)

(defun piglet-company-backend (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'piglet-company-backend))
    (prefix (let ((node (treesit-node-at (- (point) 1))))
              (when (and (eq (point) (treesit-node-end node))
                         (equal "symbol" (treesit-node-type node)))
                (treesit-node-text node))))
    (candidates
     `(:async . ,(lambda (callback)
                   (pdp-send
                    (pdp-add-handler
                     (pdp-msg `(("op" . "completion-candidates")
                                ("prefix" . ,arg)))
                     (lambda (reply)
                       (let ((cands (seq-into (pdp--msg-get reply "candidates")
                                              'list)))
                         (funcall callback cands))))))))))

(add-hook 'piglet-mode-hook (lambda () (setq-local company-backends '(piglet-company-backend))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; piglet-company.el ends here
