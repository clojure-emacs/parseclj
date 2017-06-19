;;; clj-parse.el --- Clojure/EDN parser

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

;; The EDN spec is not clear about wether \u0123 and \o012 are supported in
;; strings. They are described as character literals, but not as string escape
;; codes. In practice all implementations support them (mostly with broken
;; surrogate pair support), so we do the same. Sorry, emoji 🙁.
;;
;; Note that this is kind of broken, we don't correctly detect if \u or \o forms
;; don't have the right forms.
(defun clj-parse-string (s)
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

(defun clj-parse-character (c)
  (let* ((form (cdr (assq 'form token)))
         (first-char (elt form 1)))
    (cond
     ((equal form "\\newline") ?\n)
     ((equal form "\\return") ?\r)
     ((equal form "\\space") ?\ )
     ((equal form "\\tab") ?\t)
     ((eq first-char ?u) (string-to-number (substring form 2) 16))
     ((eq first-char ?o) (string-to-number (substring form 2) 8))
     (t first-char))))

(defun clj-parse-edn-reduce1 (stack token)
  (cl-case (cdr (assq 'type token))
    (:whitespace stack)
    (:number (cons (string-to-number (cdr (assq 'form token))) stack))
    (:nil (cons nil stack))
    (:true (cons t stack))
    (:false (cons nil stack))
    (:symbol (cons (intern (cdr (assq 'form token))) stack))
    (:keyword (cons (intern (cdr (assq 'form token))) stack))
    (:string (cons (clj-parse-string (cdr (assq 'form token))) stack))
    (:character (cons (clj-parse-character (cdr (assq 'form token))) stack))))

(defun clj-parse-edn-reduceN (stack type coll)
  (if (eq :discard type)
      stack
    (cons
     (cl-case type
       (:whitespace :ws)
       (:number coll)
       (:list (-butlast (cdr coll)))
       (:set (-butlast (cdr coll)))
       (:vector (apply #'vector (-butlast (cdr coll))))
       (:map (mapcar (lambda (pair)
                       (cons (car pair) (cadr pair)))
                     (-partition 2 (-butlast (cdr coll))))))
     stack)))

;; TODO move this to clj-lex
(defun clj-lex-token-type (token)
  (and (listp token)
       (cdr (assq 'type token))))

(defun clj-parse--reduce-coll (stack open-token coll-type reducN)
  (let ((coll nil))
    (while (and stack
                (not (eq (clj-lex-token-type (car stack)) open-token)))
      (push (pop stack) coll))
    (if (eq (clj-lex-token-type (car stack)) open-token)
        (progn
          (push (pop stack) coll)
          (funcall reduceN stack coll-type coll))
      ;; Unwound the stack without finding a matching paren: return the original stack
      (reverse list))))

(defun clj-parse-reduce (reduce1 reduceN)
  (let ((stack nil)
        (token (clj-lex-next)))

    (while (not (eq (clj-lex-token-type token) :eof))
      (message "STACK: %S" stack)
      (message "TOKEN: %S\n" token)

      (setf stack
            (if (member (clj-lex-token-type token)
                        clj-parse--leaf-tokens)
                (funcall reduce1 stack token)
              (cons token stack)))

      ;; Reduce based on the top item on the stack (collections)
      (cl-case (clj-lex-token-type (car stack))
        (:rparen (setf stack (clj-parse--reduce-coll stack :lparen :list reduceN)))
        (:rbracket (setf stack (clj-parse--reduce-coll stack :lbracket :vector reduceN)))
        (:rbrace
         (let ((open-token (-find (lambda (token)
                                    (member (clj-lex-token-type token) '(:lbrace :set)))
                                  stack)))

           (cl-case (clj-lex-token-type open-token)
             (:lbrace
              (setf stack (clj-parse--reduce-coll stack :lbrace :map reduceN)))
             (:set
              (setf stack (clj-parse--reduce-coll stack :set :set reduceN)))))))

      ;; Reduce based on top two items on the stack
      (if (not (clj-lex-token? (car stack))) ;; top is fully reduced
          (cl-case (clj-lex-token-type (cadr stack))
            (:discard (setf stack (funcall reduceN (cddr stack) :discard (-take 2 stack))))))

      (setq token (clj-lex-next)))

    (message "RESULT: %S" stack)
    stack))

(defun clj-parse ()
  (clj-parse-reduce 'clj-parse-edn-reduce1 'clj-parse-edn-reduceN))

(provide 'clj-parse)

;;; clj-parse.el ends here
