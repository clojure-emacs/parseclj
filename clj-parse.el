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
(require 'let-alist)
(require 'cl-lib)
(require 'clj-lex)

(defvar clj-parse--leaf-tokens '(:whitespace
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

(defun clj-parse--is-leaf? (el)
  (member (clj-lex-token-type el) clj-parse--leaf-tokens))

(defun clj-parse--is-node? (el)
  (a-has-key el 'subnodes))

(defun clj-parse--is-open-prefix? (el)
  (and (member (clj-lex-token-type el) '(:discard :tag))
       (not (clj-parse--is-node? el))))

(defun clj-parse--make-node (type subnodes &rest kvs)
  (apply 'a-list 'type type 'subnodes subnodes kvs))

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

(defun clj-parse--ast-reduce1 (stack leaf)
  (if (eq (clj-lex-token-type leaf) :whitespace)
      stack
    (push leaf stack)))

(defun clj-parse--ast-reduceN (stack node subnodes)
  (push
   (cl-case (clj-lex-token-type node)
     (:lparen (clj-parse--make-node :list subnodes))
     (:lbracket (clj-parse--make-node :vector subnodes))
     (:set (clj-parse--make-node :set subnodes))
     (:lbrace (clj-parse--make-node :map subnodes))
     (:discard (clj-parse--make-node :discard subnodes)))
   stack))

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

(defun clj-parse-reduce (reduce1 reduceN)
  (let ((stack nil))

    (while (not (eq (clj-lex-token-type (setq token (clj-lex-next))) :eof))
      (message "STACK: %S" stack)
      (message "TOKEN: %S\n" token)

      ;; Reduce based on the top item on the stack (collections)
      (let ((token-type (clj-lex-token-type token)))
        (cond
         ((member token-type clj-parse--leaf-tokens) (setf stack (funcall reduce1 stack token)))
         ((member token-type clj-parse--closer-tokens) (setf stack (clj-parse--reduce-coll stack token reduceN)))
         (t (push token stack))))

      ;; Reduce based on top two items on the stack (special prefixed elements)
      (seq-let [top lookup] stack
        (when (and (clj-parse--is-open-prefix? lookup)
                   (or (clj-parse--is-node? top)
                       (clj-parse--is-leaf? top))) ;; top is fully reduced
            (setf stack (funcall reduceN (cddr stack) lookup (list top))))))

    ;; reduce root
    (let ((root (clj-parse--make-node :root stack)))
      (message "RESULT: %S" root)
      root)))

(defun clj-parse ()
  (clj-parse-reduce #'clj-parse--ast-reduce1 #'clj-parse--ast-reduceN))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; To Elisp

(defun clj-parse--reduce-elisp-leaf (leaf)
  (cl-case (clj-lex-token-type leaf)
    (:number (string-to-number (alist-get 'form leaf)))
    (:nil nil)
    (:true t)
    (:false nil)
    (:symbol (intern (alist-get 'form leaf)))
    (:keyword (intern (alist-get 'form leaf)))
    (:string (clj-parse--string (alist-get 'form leaf)))
    (:character (clj-parse--character (alist-get 'form leaf)))))

(defun clj-parse--reduce-to-elisp (node)
  (if (clj-parse--is-leaf? node)
      (clj-parse--reduce-elisp-leaf node)
    (let ((subnodes (-remove (lambda (token) (eq (clj-lex-token-type token) :discard)) (alist-get 'subnodes node))))
      (cl-case (clj-lex-token-type node)
        (:root (mapcar 'clj-parse--reduce-to-elisp subnodes))
        (:list (mapcar 'clj-parse--reduce-to-elisp subnodes))
        (:vector (apply #'vector (mapcar 'clj-parse--reduce-to-elisp subnodes)))
        (:set (mapcar 'clj-parse--reduce-to-elisp subnodes))
        (:map (mapcar (lambda (pair)
                        (cons (clj-parse--reduce-to-elisp (car pair))
                              (clj-parse--reduce-to-elisp (cadr pair))))
                      (-partition 2 subnodes)))
        ;; tagged literal
        ))))

(defun clj-parse-to-elisp ()
  (clj-parse--reduce-to-elisp (clj-parse)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; To Clojure/EDN string

(defun clj-parse--reduce-string-leaf (leaf)
  (alist-get 'form leaf))

(defun clj-parse--string-with-delimiters (nodes ld rd)
  (s-concat ld
            (s-join " " (mapcar 'clj-parse--reduce-to-string nodes))
            rd))

(defun clj-parse--reduce-to-string (node)
  (if (clj-parse--is-leaf? node)
      (clj-parse--reduce-string-leaf node)
    (let ((subnodes (-remove (lambda (token) (eq (clj-lex-token-type token) :discard)) (alist-get 'subnodes node))))
      (cl-case (clj-lex-token-type node)
        (:root (clj-parse--string-with-delimiters subnodes "" ""))
        (:list (clj-parse--string-with-delimiters subnodes "(" ")"))
        (:vector (clj-parse--string-with-delimiters subnodes "[" "]"))
        (:set (clj-parse--string-with-delimiters subnodes "#{" "}"))
        (:map (clj-parse--string-with-delimiters subnodes "{" "}"))
        ;; tagged literals
        ))))

(defun clj-parse-to-string ()
  (clj-parse--reduce-to-string (clj-parse)))

(provide 'clj-parse)

;;; clj-parse.el ends here
