;;; clj-parse.el --- Clojure/EDN parser              -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Arne Brasseur

;; Author: Arne Brasseur <arne@arnebrasseur.net>
;; Keywords: lisp
;; Package-Requires: ((let-alist "1.0.5") (dash "2.12.0"))
;; Version: 0.1.0

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the Mozilla Public License Version 2.0

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
;; details.

;; You should have received a copy of the Mozilla Public License along with this
;; program. If not, see <https://www.mozilla.org/media/MPL/2.0/index.txt>.

;;; Commentary:

;; A reader for EDN data files and parser for Clojure source files.

;;; Code:

;; Before emacs 25.1 it's an ELPA package

(require 'a)
(require 's)
(require 'dash)
(require 'let-alist)
(require 'cl-lib)
(require 'clj-lex)

(defvar clj-parse--leaf-tokens '(:whitespace
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

(defvar clj-parse--closer-tokens '(:rparen
                                   :rbracket
                                   :rbrace)
  "Tokens that represent closing of an AST branch.")

(defun clj-parse--is-leaf? (node)
  (member (a-get node ':node-type) clj-parse--leaf-tokens))

(defun clj-parse--is-open-prefix? (el)
  (and (member (clj-lex-token-type el) '(:discard :tag))
       (clj-lex-token? el)))

;; The EDN spec is not clear about wether \u0123 and \o012 are supported in
;; strings. They are described as character literals, but not as string escape
;; codes. In practice all implementations support them (mostly with broken
;; surrogate pair support), so we do the same. Sorry, emoji 🙁.
;;
;; Note that this is kind of broken, we don't correctly detect if \u or \o forms
;; don't have the right forms.
(defun clj-parse--string (s)
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

(defun clj-parse--character (c)
  (let ((first-char (elt c 1)))
    (cond
     ((equal c "\\newline") ?\n)
     ((equal c "\\return") ?\r)
     ((equal c "\\space") ?\ )
     ((equal c "\\tab") ?\t)
     ((eq first-char ?u) (string-to-number (substring c 2) 16))
     ((eq first-char ?o) (string-to-number (substring c 2) 8))
     (t first-char))))

(defun clj-parse--leaf-token-value (token)
  (cl-case (clj-lex-token-type token)
    (:number (string-to-number (alist-get 'form token)))
    (:nil nil)
    (:true t)
    (:false nil)
    (:symbol (intern (alist-get 'form token)))
    (:keyword (intern (alist-get 'form token)))
    (:string (clj-parse--string (alist-get 'form token)))
    (:character (clj-parse--character (alist-get 'form token)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Shift-Reduce Parser

(defun clj-parse--find-opener (stack closer-token)
  (cl-case (clj-lex-token-type closer-token)
    (:rparen :lparen)
    (:rbracket :lbracket)
    (:rbrace (clj-lex-token-type
              (-find (lambda (token) (member (clj-lex-token-type token) '(:lbrace :set))) stack)))))

(defun clj-parse--reduce-coll (stack closer-token reduceN)
  "Reduce collection based on the top of the stack"
  (let ((opener-type (clj-parse--find-opener stack closer-token))
        (coll nil))
    (while (and stack
                (not (eq (clj-lex-token-type (car stack)) opener-type)))
      (push (pop stack) coll))

    (if (eq (clj-lex-token-type (car stack)) opener-type)
        (let ((node (pop stack)))
          (funcall reduceN stack node coll))
      ;; Syntax error
      (error "Syntax Error"))))

(defun clj-parse-reduce (reduce-leaf reduce-node)
  (let ((stack nil))

    (while (not (eq (clj-lex-token-type (setq token (clj-lex-next))) :eof))
      (message "STACK: %S" stack)
      (message "TOKEN: %S\n" token)

      ;; Reduce based on the top item on the stack (collections)
      (let ((token-type (clj-lex-token-type token)))
        (cond
         ((member token-type clj-parse--leaf-tokens) (setf stack (funcall reduce-leaf stack token)))
         ((member token-type clj-parse--closer-tokens) (setf stack (clj-parse--reduce-coll stack token reduce-node)))
         (t (push token stack))))

      ;; Reduce based on top two items on the stack (special prefixed elements)
      (seq-let [top lookup] stack
        (when (and (clj-parse--is-open-prefix? lookup)
                   (not (clj-lex-token? top))) ;; top is fully reduced
            (setf stack (funcall reduce-node (cddr stack) lookup (list top))))))

    ;; reduce root
    (setf stack (funcall reduce-node stack '((type . :root) (pos . 0)) stack))
    (message "RESULT: %S" stack)
    stack))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Reducer implementations

(defun clj-parse--make-node (type position &rest kvs)
  (apply 'a-list ':node-type type ':position position kvs))

;; AST

(defun clj-parse--ast-reduce-leaf (stack token)
  (if (eq (clj-lex-token-type token) :whitespace)
      stack
    (push
     (clj-parse--make-node (clj-lex-token-type token) (a-get token 'pos)
                           ':form (a-get token 'form)
                           ':value (clj-parse--leaf-token-value token))
     stack)))

(defun clj-parse--ast-reduce-node (stack opener-token children)
  (let* ((pos (a-get opener-token 'pos))
         (type (cl-case (clj-lex-token-type opener-token)
                 (:root :root)
                 (:lparen :list)
                 (:lbracket :vector)
                 (:set :set)
                 (:lbrace :map)
                 (:discard :discard))))
    (cl-case type
      (:root (clj-parse--make-node :root 0 ':children children))
      (:discard stack)
      (t (push
          (clj-parse--make-node type pos
                                ':children children)
          stack)))))

(defun clj-parse-ast ()
  (clj-parse-reduce #'clj-parse--ast-reduce-leaf #'clj-parse--ast-reduce-node))

; Elisp

(defun clj-parse--edn-reduce-leaf (stack token)
  (if (member (clj-lex-token-type token) (list :whitespace :comment))
      stack
    (push (clj-parse--leaf-token-value token) stack)))

(defun clj-parse--edn-reduce-node (stack opener-token children)
  (let ((type (clj-lex-token-type opener-token)))
    (if (member type '(:root :discard))
        stack
      (push
       (cl-case type
         (:lparen children)
         (:lbracket (apply #'vector children))
         (:set (list 'edn-set children))
         (:lbrace (let* ((kvs (seq-partition children 2))
                         (hash-map (make-hash-table :test 'equal :size (length kvs))))
                    (seq-do (lambda (pair)
                              (puthash (car pair) (cadr pair) hash-map))
                            kvs)
                    hash-map)))
       stack))))

(defun clj-parse-edn ()
  (clj-parse-reduce #'clj-parse--edn-reduce-leaf #'clj-parse--edn-reduce-node))

(defun clj-parse-edn-str (s)
  (with-temp-buffer
    (insert s)
    (goto-char 1)
    (car (clj-parse-reduce #'clj-parse--edn-reduce-leaf #'clj-parse--edn-reduce-node))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Printer implementations

;; AST

(defun clj-parse--reduce-string-leaf (leaf)
  (alist-get ':form leaf))

(defun clj-parse--string-with-delimiters (nodes ld rd)
  (concat ld
          (s-join " " (mapcar #'clj-parse-ast-print nodes))
          rd))

(defun clj-parse-ast-print (node)
  (if (clj-parse--is-leaf? node)
      (clj-parse--reduce-string-leaf node)
    (let ((subnodes (alist-get ':children node)))
      (cl-case (a-get node ':node-type)
        (:root (clj-parse--string-with-delimiters subnodes "" ""))
        (:list (clj-parse--string-with-delimiters subnodes "(" ")"))
        (:vector (clj-parse--string-with-delimiters subnodes "[" "]"))
        (:set (clj-parse--string-with-delimiters subnodes "#{" "}"))
        (:map (clj-parse--string-with-delimiters subnodes "{" "}"))
        ;; tagged literals
        ))))

(provide 'clj-parse)

;;; clj-parse.el ends here
