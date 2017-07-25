;;; parseedn.el --- EDN reader/writer              -*- lexical-binding: t; -*-

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

(defvar parseedn-default-tag-readers
  (a-list 'inst (lambda (s)
                  (cl-list* 'edn-inst (date-to-time s)))
          'uuid (lambda (s)
                  (list 'edn-uuid s)))
  "Default reader functions for handling tagged literals in EDN.
These are the ones defined in the EDN spec, #inst and #uuid. It
is not recommended you change this variable, as this globally
changes the behavior of the EDN reader. Instead pass your own
handlers as an optional argument to the reader functions.")

(defun parseedn-reduce-leaf (stack token)
  (if (member (parseclj-lex-token-type token) (list :whitespace :comment))
      stack
    (cons (parseclj--leaf-token-value token) stack)))

(defun parseedn-reduce-branch (tag-readers)
  (lambda (stack opener-token children)
    (let ((token-type (parseclj-lex-token-type opener-token)))
      (if (member token-type '(:root :discard))
          stack
        (cons
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

(defun parseedn-read (&optional tag-readers)
  (parseclj-parse #'parseedn-reduce-leaf
                    (parseedn-reduce-branch (a-merge parseedn-default-tag-readers tag-readers))))

(defun parseedn-read-str (s &optional tag-readers)
  (with-temp-buffer
    (insert s)
    (goto-char 1)
    (car (parseedn-read tag-readers))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Printer


(defun parseedn-print-seq (coll)
  (parseedn-print (elt coll 0))
  (let ((next (seq-drop coll 1)))
    (when (not (seq-empty-p next))
      (insert " ")
      (parseedn-print-seq next))))

(defun parseedn-print-kvs (map)
  (let ((keys (a-keys map)))
    (parseedn-print (car keys))
    (insert " ")
    (parseedn-print (a-get map (car keys)))
    (let ((next (cdr keys)))
      (when (not (seq-empty-p next))
        (insert ", ")
        (parseedn-print-kvs next)))))

(defun parseedn-print (datum)
  (cond
   ((or (null datum) (numberp datum))
    (prin1 datum (current-buffer)))

   ((stringp datum)
    (insert "\"")
    (seq-doseq (char datum)
      (insert (cl-case char
                (?\t "\\t")
                (?\f "\\f")
                (?\" "\\\"")
                (?\r "\\r")
                (?\n"foo\t" "\\n")
                (?\\ "\\\\")
                (t (char-to-string char)))))
    (insert "\""))

   ((eq t datum)
    (insert "true"))

   ((symbolp datum)
    (insert (symbol-name datum)))

   ((vectorp datum) (insert "[") (parseedn-print-seq datum) (insert "]"))

   ((consp datum)
    (cond
     ((eq 'edn-set (car datum))
      (insert "#{") (parseedn-print-seq (cadr datum)) (insert "}"))
     (t (insert "(") (parseedn-print-seq datum) (insert ")"))))

   ((hash-table-p datum)
    (insert "{") (parseedn-print-kvs datum) (insert "}"))))

(defun parseedn-print-str (datum)
  (with-temp-buffer
    (parseedn-print datum)
    (buffer-substring-no-properties (point-min) (point-max))))

(provide 'parseedn)

;;; parseedn.el ends here
