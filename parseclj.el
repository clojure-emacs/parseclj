;;; parseclj.el --- Clojure/EDN parser              -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2018  Arne Brasseur

;; Author: Arne Brasseur <arne@arnebrasseur.net>
;; Keywords: lisp clojure edn parser
;; Package-Requires: ((emacs "25") (a "0.1.0alpha4"))
;; Version: 0.1.0

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

;; Top level API for the Clojure parser.

;;; Code:

(require 'parseclj-parser)
(require 'parseclj-ast)

(defun parseclj-alist (&rest kvs)
  "Create an association list from the given keys and values KVS.
Arguments are simply provided in sequence, rather than as lists or cons cells.
For example: (a-alist :foo 123 :bar 456)"
  (mapcar (lambda (kv) (cons (car kv) (cadr kv))) (seq-partition kvs 2)))

(defun parseclj-hash-table (&rest kvs)
  "Create a hash table from the given keys and values KVS.
Arguments are simply provided in sequence, rather than as lists
or cons cells. As \"test\" for the hash table, equal is used. The
hash table is created without extra storage space, so with a size
equal to amount of key-value pairs, since it is assumed to be
treated as immutable.
For example: (a-hash-table :foo 123 :bar 456)"
  (let* ((kv-pairs (seq-partition kvs 2))
         (hash-map (make-hash-table :test 'equal :size (length kv-pairs))))
    (seq-do (lambda (pair)
              (puthash (car pair) (cadr pair) hash-map))
            kv-pairs)
    hash-map))

(defun parseclj-alist-get (map key &optional not-found)
  "Like alist-get, but uses equal instead of eq to look up in map MAP key KEY.
Returns NOT-FOUND if the key is not present, or `nil' if
NOT-FOUND is not specified."
  (cl-block nil
    (seq-doseq (pair map)
      (when (equal (car pair) key)
        (cl-return (cdr pair))))
    not-found))

(defun parseclj-alist-has-key? (coll k)
  "Check if the given association list COLL has a certain key K."
  (not (eq (parseclj-alist-get coll k :not-found) :not-found)))

(defun parseclj-alist-assoc (coll k v)
  (if (parseclj-alist-has-key? coll k)
      (mapcar (lambda (entry)
                (if (equal (car entry) k)
                    (cons k v)
                  entry))
              coll)
    (cons (cons k v) coll)))

(defun parseclj-alist-update (coll key fn &rest args)
  "In collection COLL, at location KEY, apply FN with extra args ARGS.
'Updates' a value in an associative collection COLL, where KEY is
a key and FN is a function that will take the old value and any
supplied args and return the new value, and returns a new
structure. If the key does not exist, nil is passed as the old
value."
  (parseclj-alist-assoc coll
                        key
                        (apply #'funcall fn (parseclj-alist-get coll key) args)))

(defun parseclj-parse-clojure (&rest string-and-options)
  "Parse Clojure source to AST.

Reads either from the current buffer, starting from point, until
`point-max', or reads from the optional string argument.

STRING-AND-OPTIONS can be an optional string, followed by
key-value pairs to specify parsing options.

- `:lexical-preservation' Retain whitespace, comments, and
  discards.  Defaults to nil.
- `:fail-fast' Raise an error when encountering invalid syntax.
  Defaults to t.
- `:read-one'
  Read a single form.  Defaults to false: parse the complete input."
  (if (stringp (car string-and-options))
      (with-temp-buffer
        (insert (car string-and-options))
        (goto-char 1)
        (apply 'parseclj-parse-clojure (cdr string-and-options)))
    (let* ((value-p (lambda (e)
                      (and (parseclj-ast-node-p e)
                           (not (member (parseclj-ast-node-type e) '(:whitespace :comment :discard))))))
           (options (apply 'parseclj-alist :value-p value-p string-and-options))
           (lexical? (parseclj-alist-get options :lexical-preservation)))
      (parseclj-parser (if lexical?
                           #'parseclj-ast--reduce-leaf-with-lexical-preservation
                         #'parseclj-ast--reduce-leaf)
                       (if lexical?
                           #'parseclj-ast--reduce-branch-with-lexical-preservation
                         #'parseclj-ast--reduce-branch)
                       options))))

(defun parseclj-unparse-clojure (ast)
  "Parse Clojure AST to source code.

Given an abstract syntax tree AST (as returned by
`parseclj-parse-clojure'), turn it back into source code, and
insert it into the current buffer."
  (if (parseclj-ast-leaf-node-p ast)
      (insert (parseclj-alist-get ast :form))
    (if (eql (parseclj-ast-node-type ast) :tag)
        (parseclj-ast--unparse-tag ast)
      (parseclj-ast--unparse-collection ast))))

(defun parseclj-unparse-clojure-to-string (ast)
  "Parse Clojure AST to a source code string.

Given an abstract syntax tree AST (as returned by
`parseclj-parse-clojure'), turn it back into source code, and
return it as a string"
  (with-temp-buffer
    (parseclj-unparse-clojure ast)
    (buffer-substring-no-properties (point-min) (point-max))))

(provide 'parseclj)

;;; parseclj.el ends here
