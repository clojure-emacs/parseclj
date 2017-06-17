;;; clj-parse.el --- Clojure/EDN parser

;; Copyright (C) 2017  Arne Brasseur

;; Author: Arne Brasseur <arne@arnebrasseur.net>
;; Keywords: lisp
;; Package-Requires: ((let-alist "1.0.5"))
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

(require 'cl-lib)

;; Before emacs 25.1 it's an ELPA package
(require 'let-alist)

(require 'clj-lex)

(defun clj-parse-edn-reduce1 (stack token)
  (cl-case (cdr (assq 'type token))
    (:whitespace stack)
    (:number (cons (string-to-number (cdr (assq 'form token))) stack))))

(defun clj-parse-edn-reduceN (stack type coll)
  (cons
   (cl-case type
     (:whitespace :ws)
     (:number coll)
     (:list (-butlast (cdr coll))))
   stack))

(defvar clj-parse--terminal-tokens '(:whitespace :number))


(defun clj-parse--token-type (token)
  (and (listp token) (cdr (assq 'type token))))

(defun clj-parse--unwind-stack (stack target)
  (let ((result nil))))

(defun clj-parse--reduce-list (stack reducN)
  (let ((coll nil))
    (while (and stack (not (eq (clj-parse--token-type (car stack)) :lparen)))
      (push (pop stack) coll))
    (if (eq (clj-parse--token-type (car stack)) :lparen)
        (progn
          (push (pop stack) coll)
          (funcall reduceN stack :list coll))
      ;; Unwound the stack without finding a matching paren: return the original stack
      (reverse list))))

(defun clj-parse-reduce (reduce1 reduceN)
  (let ((stack nil)
        (token (clj-lex-next)))

    (while (not (eq (clj-parse--token-type token) :eof))
      (message "STACK: %S" stack)
      (message "TOKEN: %S\n" token)

      (setf stack
            (if (member (clj-parse--token-type token) clj-parse--terminal-tokens)
                (funcall reduce1 stack token)
              (cons token stack)))

      (cl-case (clj-parse--token-type (car stack))
        (:rparen (setf stack (clj-parse--reduce-list stack reduceN))))

      (setq token (clj-lex-next)))

    (message "RESULT: %S" stack)
    stack))

(defun clj-parse ()
  (clj-parse-reduce 'clj-parse-edn-reduce1 'clj-parse-edn-reduceN))

(provide 'clj-parse)
;;; clj-parse.el ends here
