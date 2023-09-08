;;; cbor.el --- CBOR utilities -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Óscar Nájera, Arne Brasseur
;;
;; Author: Oscar Najera <https://oscarnajera.com>
;; Maintainer: Arne Brasseur
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Utility to decode and encode CBOR, forked from cbor.el.
;;
;;; Code:

(require 'hex-util)
(require 'cl-lib)
(require 'json)
(require 'seq)

(cl-defstruct (piglet-cbor-tag (:constructor piglet-cbor-tag-create)
                               (:copier nil))
  number content)

(defun piglet-cbor--get-ints (string &optional little)
  "Convert byte STRING to integer.
Default to big-endian unless LITTLE is non-nil."
  (seq-reduce (lambda (acc n) (logior n (ash acc 8))) (if little (reverse string) string) 0))

(defun piglet-cbor--luint (value size &optional little)
  "Convert VALUE into list of SIZE bytes.
Default to big endian unless LITTLE is non-nil."
  (let ((bytes
         (cl-loop for num = value then (ash num -8)
                  repeat size
                  collect (logand num #xff))))
    (if little bytes (nreverse bytes))))

(defun piglet-cbor--consume! (bytes)
  "Consume N BYTES from the source string."
  (prog1 (buffer-substring (point) (+ (point) bytes))
    (forward-char bytes)))

(defun piglet-cbor--get-argument! (info)
  "Get argument meaning given INFO."
  (cond
   ((< info 24) info)
   ((= info 24) (piglet-cbor--get-ints (piglet-cbor--consume! 1)))
   ((= info 25) (piglet-cbor--get-ints (piglet-cbor--consume! 2)))
   ((= info 26) (piglet-cbor--get-ints (piglet-cbor--consume! 4)))
   ((= info 27) (piglet-cbor--get-ints (piglet-cbor--consume! 8)))
   ((= info 31) 'indefinite-length)
   (t 'malformed-input)))

(defun piglet-cbor--get-data-array! (item-count)
  "Get ITEM-COUNT number of array elements."
  (let (result)
    (if (eq 'indefinite-length item-count)
        (let ((value (piglet-cbor--get-data-item!)))
          (while (not (eq value 'break))
            (push value result)
            (setq value (piglet-cbor--get-data-item!))))
      (dotimes (_ item-count)
        (push (piglet-cbor--get-data-item!) result)))
    (vconcat (nreverse result))))

(defun piglet-cbor--get-data-map! (pair-count)
  "Get PAIR-COUNT number of map elements."
  (let (result)
    (if (eq 'indefinite-length pair-count)
        (let ((key (piglet-cbor--get-data-item!)))
          (while (not (eq key 'break))
            (push (cons key (piglet-cbor--get-data-item!)) result)
            (setq key (piglet-cbor--get-data-item!))))
      (dotimes (_ pair-count)
        (push (cons (piglet-cbor--get-data-item!) (piglet-cbor--get-data-item!)) result)))
    (nreverse result)))

(defun piglet-cbor--get-data-item! ()
  "Read a single CBOR data item."
  (let* ((initial-byte (string-to-char (piglet-cbor--consume! 1)))
         ;; major type is in the higher-order 3 bits
         (major-type (ash initial-byte -5))
         ;; additional information is in the lower-order 5 bits (string-to-number "00011111" 2) =>31
         (additional-information (logand initial-byte 31))
         (argument (piglet-cbor--get-argument! additional-information)))
    (pcase major-type
      ;; unsigned integer
      (0 argument)
      ;; negative integer
      (1 (- -1 argument))
      ;; bytestring are hex encoded
      (2 (encode-hex-string (piglet-cbor--consume! argument)))
      ;; Text strings
      (3 (decode-coding-string (piglet-cbor--consume! argument) 'utf-8))
      ;; array of data
      (4 (piglet-cbor--get-data-array! argument))
      ;; map of pairs of data items
      (5 (piglet-cbor--get-data-map! argument))
      ;; cbor tag
      (6
       (if (equal 39 argument)
           (intern (piglet-cbor--get-data-item!))
         (piglet-cbor-tag-create :number argument :content (piglet-cbor--get-data-item!))))
      ;; simple values
      (7
       (pcase additional-information
         (20 :false) ;; False
         (21 t)
         (22 :null) ;; NULL
         (31 'break))))))

(defun piglet-cbor-hex-p (str)
  "Test if STR is a hex only."
  (string-match-p (rx line-start (+ hex) line-end) str))

(defun piglet-cbor->elisp (byte-or-hex-string)
  "Convert BYTE-OR-HEX-STRING into an Emacs Lisp object."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert (if (and (zerop (mod (length byte-or-hex-string) 2))
                     (piglet-cbor-hex-p byte-or-hex-string))
                (decode-hex-string byte-or-hex-string)
              byte-or-hex-string))
    (goto-char (point-min))
    (piglet-cbor--get-data-item!)))

;; Encoding
(defun piglet-cbor<-elisp (object)
  "Convert Emacs Lisp OBJECT into cbor encoded hex-string."
  (with-temp-buffer
    (let ((standard-output (current-buffer)))
      (set-buffer-multibyte nil)
      (piglet-cbor--put-data-item! object))
    (buffer-string)))

(defun piglet-cbor--put-initial-byte! (major-type additional-information)
  "Encode and push the first byte giving MAJOR-TYPE and ADDITIONAL-INFORMATION."
  (write-char
   (logior (ash major-type 5) additional-information)))

(defun piglet-cbor-uint-needed-size (uint)
  "Return standard required sized to store UINT up to 8 bytes."
  (pcase (ceiling (log (1+ uint) 256))
    (1 1)
    (2 2)
    ((or 3 4) 4)
    ((or 5 6 7 8) 8)
    (_ (error "Interger to large to encode"))))

(defun piglet-cbor--put-ints! (major-type uint)
  "Push to the out stream the defined MAJOR-TYPE with quantity UINT."
  (if (< uint 24)
      (piglet-cbor--put-initial-byte! major-type uint)
    (let ((size (piglet-cbor-uint-needed-size uint)))
      (pcase size
        (1 (piglet-cbor--put-initial-byte! major-type 24))
        (2 (piglet-cbor--put-initial-byte! major-type 25))
        (4 (piglet-cbor--put-initial-byte! major-type 26))
        (8 (piglet-cbor--put-initial-byte! major-type 27)))
      (mapc #'write-char (piglet-cbor--luint uint size)))))

(defun piglet-cbor--put-data-item! (value)
  "Encode Lisp VALUE into cbor list."
  (cond
   ((and (integerp value) (<= 0 value))
    (piglet-cbor--put-ints! 0 value))

   ((and (integerp value) (> 0 value))
    (piglet-cbor--put-ints! 1 (- -1 value)))

   ;; bytestring are hex encoded
   ((and (stringp value) (zerop (mod (length value) 2)) (piglet-cbor-hex-p value))
    (let ((rev-list
           (decode-hex-string value)))
      (piglet-cbor--put-ints! 2 (length rev-list))
      (mapc #'write-char (string-to-list rev-list))))

   ;; Text strings
   ((stringp value)
    (progn
      (piglet-cbor--put-ints! 3 (string-bytes value))
      (princ (encode-coding-string value 'utf-8))))

   ;; array of data
   ((vectorp value)
    (progn
      (piglet-cbor--put-ints! 4 (length value))
      (mapc #'cbor--put-data-item! value)))

   ;; map of pairs of data items
   ((json-alist-p value)
    (progn
      (piglet-cbor--put-ints! 5 (length value))
      (mapc (lambda (key-value)
              (piglet-cbor--put-data-item! (car key-value))
              (piglet-cbor--put-data-item! (cdr key-value)))
            value)))

   ;; Cbor Tags
   ((piglet-cbor-tag-p value)
    (progn
      (piglet-cbor--put-ints! 6 (piglet-cbor-tag-number value))
      (piglet-cbor--put-data-item! (piglet-cbor-tag-content value))))

   ((symbolp value)
    (progn
      (piglet-cbor--put-ints! 6 39)
      (piglet-cbor--put-data-item! (symbol-name value))))

   ;; simple values
   ((eq 'false value)
    (piglet-cbor--put-initial-byte! 7 20))
   ((eq t value)
    (piglet-cbor--put-initial-byte! 7 21))
   ((eq nil value)
    (piglet-cbor--put-initial-byte! 7 22))

   (t
    (error (format "Can't encode %S" value)))))

(provide 'piglet-cbor)
;;; piglet-cbor.el ends here
