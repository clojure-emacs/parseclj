;;; parseclj.el --- Clojure/EDN parser              -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Arne Brasseur

;; Author: Arne Brasseur <arne@arnebrasseur.net>
;; Keywords: lisp
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

;; A reader for EDN data files and parser for Clojure source files.

;;; Code:

(require 'cl-lib)
(require 'a)

(require 'parseclj-lex)
(require 'parseedn)
(require 'parseclj-ast)

(defvar parseclj--leaf-tokens '(:whitespace
                                :comment
                                :number
                                :nil
                                :true
                                :false
                                :symbol
                                :keyword
                                :string
                                :character)
  "Tokens that represent leaf nodes in the AST.")

(defvar parseclj--closer-tokens '(:rparen
                                  :rbracket
                                  :rbrace)
  "Tokens that represent closing of an AST branch.")

(defun parseclj--leaf? (node)
  "Return `t' if the given ast NODE is a leaf node."
  (member (a-get node ':node-type) parseclj--leaf-tokens))

;; The EDN spec is not clear about wether \u0123 and \o012 are supported in
;; strings. They are described as character literals, but not as string escape
;; codes. In practice all implementations support them (mostly with broken
;; surrogate pair support), so we do the same. Sorry, emoji 🙁.
;;
;; Note that this is kind of broken, we don't correctly detect if \u or \o forms
;; don't have the right forms.
(defun parseclj--string (s)
  (replace-regexp-in-string
   "\\\\o[0-8]\\{3\\}"
   (lambda (x)
     (make-string 1 (string-to-number (substring x 2) 8) ))
   (replace-regexp-in-string
    "\\\\u[0-9a-fA-F]\\{4\\}"
    (lambda (x)
      (make-string 1 (string-to-number (substring x 2) 16)))
    (replace-regexp-in-string "\\\\[tbnrf'\"\\]"
                              (lambda (x)
                                (cl-case (elt x 1)
                                  (?t "\t")
                                  (?f "\f")
                                  (?\" "\"")
                                  (?r "\r")
                                  (?n "\n")
                                  (?\\ "\\\\")
                                  (t (substring x 1))))
                              (substring s 1 -1)))))

(defun parseclj--character (c)
  (let ((first-char (elt c 1)))
    (cond
     ((equal c "\\newline") ?\n)
     ((equal c "\\return") ?\r)
     ((equal c "\\space") ?\ )
     ((equal c "\\tab") ?\t)
     ((eq first-char ?u) (string-to-number (substring c 2) 16))
     ((eq first-char ?o) (string-to-number (substring c 2) 8))
     (t first-char))))

(defun parseclj--leaf-token-value (token)
  (cl-case (parseclj-lex-token-type token)
    (:number (string-to-number (alist-get 'form token)))
    (:nil nil)
    (:true t)
    (:false nil)
    (:symbol (intern (alist-get 'form token)))
    (:keyword (intern (alist-get 'form token)))
    (:string (parseclj--string (alist-get 'form token)))
    (:character (parseclj--character (alist-get 'form token)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Shift-Reduce Parser

(defun parseclj--find-opener (stack closer-token)
  (cl-case (parseclj-lex-token-type closer-token)
    (:rparen :lparen)
    (:rbracket :lbracket)
    (:rbrace (parseclj-lex-token-type
              (seq-find (lambda (token) (member (parseclj-lex-token-type token) '(:lbrace :set))) stack)))))

(defun parseclj--reduce-coll (stack closer-token reduceN)
  "Reduce collection based on the top of the stack"
  (let ((opener-type (parseclj--find-opener stack closer-token))
        (coll nil))
    (while (and stack
                (not (eq (parseclj-lex-token-type (car stack)) opener-type)))
      (push (pop stack) coll))

    (if (eq (parseclj-lex-token-type (car stack)) opener-type)
        (let ((node (pop stack)))
          (funcall reduceN stack node coll))
      ;; Syntax error
      (progn
        (message "STACK: %S , CLOSER: %S" stack closer-token)
        (error "Syntax Error")))))

(defun parseclj-parse (reduce-leaf reduce-branch &optional options)
  "Clojure/EDN stack-based shift-reduce parser.

REDUCE-LEAF does reductions for leaf nodes. It is a function that
takes the current value of the stack and a token, and either
returns an updated stack, with a new leaf node at the
top (front), or returns the stack unmodified.

REDUCE-BRANCH does reductions for branch nodes. It is a function
that takes the current value of the stack, the type of branch
node to create, and a list of child nodes, and returns an updated
stack, with the new node at the top (front).

What \"node\" means in this case is up to the reducing functions,
it could be AST nodes (as in the case of
`parseclj-parse-clojure'), or plain values/sexps (as in the case
of `parseedn-read'), or something else. The only requirement is
that they should not put raw tokens back on the stack, as the
parser relies on the presence or absence of these to detect parse
errors.

OPTIONS is an association list which is passed on to the reducing
functions.
"
  (let ((stack nil))

    (while (not (eq (parseclj-lex-token-type (setq token (parseclj-lex-next))) :eof))
      ;; (message "STACK: %S" stack)
      ;; (message "TOKEN: %S\n" token)

      ;; Reduce based on the top item on the stack (collections)
      (let ((token-type (parseclj-lex-token-type token)))
        (cond
         ((member token-type parseclj--leaf-tokens) (setf stack (funcall reduce-leaf stack token)))
         ((member token-type parseclj--closer-tokens) (setf stack (parseclj--reduce-coll stack token reduce-branch)))
         (t (push token stack))))

      ;; Reduce based on top two items on the stack (special prefixed elements)
      (seq-let [top lookup] stack
        (when (and (parseclj-lex-token? lookup)
                   (not (parseclj-lex-token? top)) ;; top is fully reduced
                   (member (parseclj-lex-token-type lookup) '(:discard :tag)))
          (setf stack (funcall reduce-branch (cddr stack) lookup (list top))))))

    ;; reduce root
    (setf stack (funcall reduce-branch stack '((type . :root) (pos . 1)) stack))
    ;; (message "RESULT: %S" stack)
    stack))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Top level API

(defun parseclj-parse-clojure (&rest string-and-options)
  "Parse Clojure source to AST.

Reads either from the current buffer, starting from point, until
point-max, or reads from the optional string argument.

STRING-AND-OPTIONS can be an optional string, followed by
key-value pairs to specify parsing options.

- `:lexical-preservation' Retain whitespace, comments, and
  discards. Defaults to false (`nil').
- `:fail-fast' Raise an error
  when encountering invalid syntax. Defaults to true (`t'). "
  (if (stringp (car string-and-options))
      (with-temp-buffer
        (insert (car string-and-options))
        (goto-char 1)
        (apply 'parseclj-parse-clojure (cdr string-and-options)))
    (parseclj-parse #'parseclj-ast--reduce-leaf
                    #'parseclj-ast--reduce-branch
                    (apply 'a-list string-and-options))))


(provide 'parseclj)

;;; parseclj.el ends here
