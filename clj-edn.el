;;; clj-edn.el --- EDN reader/writer              -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Arne Brasseur

;; Author: Arne Brasseur <arne@arnebrasseur.net>

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; The EDN <-> Elisp reader and printer

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reader

(defvar clj-edn-default-tag-readers
  (a-list 'inst (lambda (s)
                  (cl-list* 'edn-inst (date-to-time s)))
          'uuid (lambda (s)
                  (list 'edn-uuid s)))
  "Default reader functions for handling tagged literals in EDN.
These are the ones defined in the EDN spec, #inst and #uuid. It
is not recommended you change this variable, as this globally
changes the behavior of the EDN reader. Instead pass your own
handlers as an optional argument to the reader functions.")

(defun clj-edn-reduce-leaf (stack token)
  (if (member (clj-lex-token-type token) (list :whitespace :comment))
      stack
    (push (clj-parse--leaf-token-value token) stack)))

(defun clj-edn-reduce-node (tag-readers)
  (lambda (stack opener-token children)
    (let ((token-type (clj-lex-token-type opener-token)))
      (if (member token-type '(:root :discard))
          stack
        (push
         (cl-case token-type
           (:lparen children)
           (:lbracket (apply #'vector children))
           (:set (list 'edn-set children))
           (:lbrace (let* ((kvs (seq-partition children 2))
                           (hash-map (make-hash-table :test 'equal :size (length kvs))))
                      (seq-do (lambda (pair)
                                (puthash (car pair) (cadr pair) hash-map))
                              kvs)
                      hash-map))
           (:tag (let* ((tag (intern (substring (a-get opener-token 'form) 1)))
                        (reader (a-get tag-readers tag :missing)))
                   (when (eq :missing reader)
                     (user-error "No reader for tag #%S in %S" tag (a-keys tag-readers)))
                   (funcall reader (car children)))))
         stack)))))

(defun clj-edn-read (&optional tag-readers)
  (clj-parse-reduce #'clj-edn-reduce-leaf
                    (clj-edn-reduce-node (a-merge clj-edn-default-tag-readers tag-readers))))

(defun clj-edn-read-str (s &optional tag-readers)
  (with-temp-buffer
    (insert s)
    (goto-char 1)
    (car (clj-edn-read tag-readers))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Printer


(provide 'clj-edn)

;;; clj-edn.el ends here
