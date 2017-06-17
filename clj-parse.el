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

(defun clj-parse ()
  (clj-parse-reduce 'clj-parse-edn-reduce1 'clj-parse-edn-reduceN))

(defun clj-parse-edn-reduce1 (stack token)
  )

(defun clj-parse-edn-reduceN (stack type coll)
  (cl-case type
    (:whitespace :ws)
    (:number coll)
    (:list coll)))

(defun clj-parse-terminal? (token)
  (cdr (assq ('type token)))
  )

(defun clj-parse-reduce (reduce1 reducer)
  (let ((stack nil)
        (token (clj-lex-next)))
    (while (not (eq (cdr (assq 'type token)) :eof))
      ;; (prin1 (alist-get 'type token))
      ;; (print token)
      ;; (print stack)
      (let-alist token
        (setf stack
              (if (clj-parse-terminal? token)
                  ))
        (cl-case .type
          (:whitespace
           (push (funcall reducer stack :whitespace .form) stack))
          (:number
           (push (funcall reducer stack :number .value) stack))
          (:lparen
           (push token stack))
          (:rparen
           (let ((list nil))
             (while (not (and (listp (car stack)) (eq (cdr (assq 'type (car stack))) :lparen)))
               (push (pop stack) list))
             (pop stack) ;; :lparen
             ;; (print list)
             (push (funcall reducer stack :list list) stack)))))
      (setq token (clj-lex-next)))
    stack))

(provide 'clj-parse)
;;; clj-parse.el ends here
