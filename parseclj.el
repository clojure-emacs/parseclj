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
  "Types of tokens that represent leaf nodes in the AST.")

(defvar parseclj--closing-tokens '(:rparen
                                   :rbracket
                                   :rbrace)
  "Types of tokens that mark the end of a non-atomic form.")

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
    (:number (string-to-number (alist-get :form token)))
    (:nil nil)
    (:true t)
    (:false nil)
    (:symbol (intern (alist-get :form token)))
    (:keyword (intern (alist-get :form token)))
    (:string (parseclj--string (alist-get :form token)))
    (:character (parseclj--character (alist-get :form token)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Shift-Reduce Parser

(define-error 'parseclj-parse-error "parseclj: Syntax error")

(defun parseclj--error (format &rest args)
  "Signal a parse error.
Takes a FORMAT string and optional ARGS to be passed to
`format-message'. Signals a 'parseclj-parse-error signal, which
can be handled with `condition-case'."
  (signal 'parseclj-parse-error (list (apply #'format-message format args))))

(defun parseclj--find-opening-token (stack closing-token)
  (cl-case (parseclj-lex-token-type closing-token)
    (:rparen :lparen)
    (:rbracket :lbracket)
    (:rbrace (parseclj-lex-token-type
              (seq-find (lambda (token)
                          (member (parseclj-lex-token-type token)
                                  '(:lbrace :set)))
                        stack)))))

(defun parseclj--reduce-coll (stack closing-token reduce-branch options)
  "Reduce collection based on the top of the stack"
  (let ((opening-token-type (parseclj--find-opening-token stack closing-token))
        (fail-fast (a-get options :fail-fast t))
        (collection nil))

    ;; unwind the stack until opening-token-type is found, adding to collection
    (while (and stack (not (eq (parseclj-lex-token-type (car stack)) opening-token-type)))
      (push (pop stack) collection))

    ;; did we find the right token?
    (if (eq (parseclj-lex-token-type (car stack)) opening-token-type)
        (progn
          (when fail-fast
            ;; any unreduced tokens left: bail early
            (when-let ((token (seq-find #'parseclj-lex-token? collection)))
              (parseclj--error "parseclj: Syntax Error at position %s, unmatched %S"
                               (a-get token :pos)
                               (parseclj-lex-token-type token))))

          ;; all good, call the reducer so it can return an updated stack with a
          ;; new node at the top.
          (let ((opening-token (pop stack)))
            (funcall reduce-branch stack opening-token collection options)))

      ;; Unwound the stack without finding a matching paren: either bail early
      ;; or return the original stack and continue parsing
      (if fail-fast
          (parseclj--error "parseclj: Syntax Error at position %s, unmatched %S"
                           (a-get closing-token :pos)
                           (parseclj-lex-token-type closing-token))

        (reverse collection)))))

(defun parseclj--take-value (stack value-p)
  "Scan until a value is found.
Return everything up to the value in reversed order (meaning the
value comes first in the result).

STACK is the current parse stack to scan.

VALUE-P a predicate to distinguish reduced values from
non-values (tokens and whitespace)."
  (let ((result nil))
    (cl-block nil
      (while stack
        (cond
         ((parseclj-lex-token? (car stack))
          (cl-return nil))

         ((funcall value-p (car stack))
          (cl-return (cons (car stack) result)))

         (t
          (push (pop stack) result)))))))

(defun parseclj--take-token (stack value-p token-types)
  "Scan until a token of a certain type is found.
Returns nil if a value is encountered before a matching token is
found. Return everything up to the token in reversed
order (meaning the token comes first in the result).

STACK is the current parse stack to scan.

VALUE-P a predicate to distinguish reduced values from
non-values (tokens and whitespace).

TOKEN-TYPES are the token types to look for."
  (let ((result nil))
    (cl-block nil
      (while stack
        (cond
         ((member (parseclj-lex-token-type (car stack)) token-types)
          (cl-return (cons (car stack) result)))

         ((funcall value-p (car stack))
          (cl-return nil))

         ((parseclj-lex-token? (car stack))
          (cl-return nil))

         (t
          (push (pop stack) result)))))))

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
functions. Additionally the following options are recognized

- :fail-fast
  Raise an error when a parse error is encountered, rather than
  continuing with a partial result.
- :value-p
  A predicate function to differentiate values from tokens and
  whitespace. This is needed when scanning the stack to see if
  any reductions can be performed. By default anything that isn't
  a token is considered a value. This can be problematic when
  parsing with `:lexical-preservation', and which case you should
  provide an implementation that also returns falsy for
  :whitespace, :comment, and :discard AST nodes. "
  (let ((fail-fast (a-get options :fail-fast t))
        (value-p (a-get options :value-p (lambda (e) (not (parseclj-lex-token? e)))))
        (stack nil)
        (token (parseclj-lex-next)))

    (while (not (eq (parseclj-lex-token-type token) :eof))
      ;; (message "STACK: %S" stack)
      ;; (message "TOKEN: %S\n" token)

      ;; Reduce based on the top item on the stack (collections)
      (cond
       ((parseclj-lex-leaf-token? token)
        (setf stack (funcall reduce-leaf stack token options)))

       ((parseclj-lex-closing-token? token)
        (setf stack (parseclj--reduce-coll stack token reduce-branch options)))

       (t (push token stack)))

      ;; Reduce based on top two items on the stack (special prefixed elements)
      (let* ((top-value (parseclj--take-value stack value-p))
             (opening-token (parseclj--take-token (nthcdr (length top-value) stack) value-p '(:discard :tag)))
             (new-stack (nthcdr (+ (length top-value) (length opening-token)) stack)))
        (when (and top-value opening-token)
          ;; (message "Reducing...")
          ;; (message "  - STACK %S" stack)
          ;; (message "  - OPENING_TOKEN %S" opening-token)
          ;; (message "  - TOP_VALUE %S\n" top-value)
          (setq stack (funcall reduce-branch new-stack (car opening-token) (append (cdr opening-token) top-value) options))))

      (setq token (parseclj-lex-next)))

    ;; reduce root
    (when fail-fast
      (when-let ((token (seq-find #'parseclj-lex-token? stack)))
        (parseclj--error "parseclj: Syntax Error at position %s, unmatched %S"
                         (a-get token :pos)
                         (parseclj-lex-token-type token))))

    (car (funcall reduce-branch nil (parseclj-lex-token :root "" 1)
                  (reverse stack)
                  options))))

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
    (let* ((value-p (lambda (e)
                      (and (parseclj-ast-node? e)
                           (not (member (parseclj-ast-node-type e) '(:whitespace :comment :discard))))))
           (options (apply 'a-list :value-p value-p string-and-options))
           (lexical? (a-get options :lexical-preservation)))
      (parseclj-parse (if lexical?
                          #'parseclj-ast--reduce-leaf-with-lexical-preservation
                        #'parseclj-ast--reduce-leaf)
                      (if lexical?
                          #'parseclj-ast--reduce-branch-with-lexical-preservation
                        #'parseclj-ast--reduce-branch)
                      options))))

(defun parseclj-unparse-clojure (ast)
  "Parse Clojure AST to source code.

Given an abstract syntax tree AST (as returned by
parseclj-parse-clojure), turn it back into source code, and
insert it into the current buffer."
  (if (parseclj-ast-leaf-node? ast)
      (insert (a-get ast :form))
    (cl-case (parseclj-ast-node-type ast)
      (:root (parseclj-unparse--collection ast "" ""))
      (:list (parseclj-unparse--collection ast "(" ")"))
      (:vector (parseclj-unparse--collection ast "[" "]"))
      (:set (parseclj-unparse--collection ast "#{" "}"))
      (:map (parseclj-unparse--collection ast "{" "}"))
      (:tag (parseclj-unparse--tag ast)))))

(defun parseclj-unparse-clojure-to-string (ast)
  "Parse Clojure AST to a source code string.

Given an abstract syntax tree AST (as returned by
parseclj-parse-clojure), turn it back into source code, and
return it as a string"
  (with-temp-buffer
    (parseclj-unparse-clojure ast)
    (buffer-substring-no-properties (point-min) (point-max))))

(provide 'parseclj)

;;; parseclj.el ends here
