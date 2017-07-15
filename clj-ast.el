;;; clj-ast.el --- Clojure parser/unparser              -*- lexical-binding: t; -*-

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

;; Parse Clojure code to an AST, and unparse back to code.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Parser

(defun clj-parse--make-node (type position &rest kvs)
  (apply 'a-list ':node-type type ':position position kvs))

(defun clj-ast--reduce-leaf (stack token)
  (if (eq (clj-lex-token-type token) :whitespace)
      stack
    (cons
     (clj-parse--make-node (clj-lex-token-type token) (a-get token 'pos)
                           ':form (a-get token 'form)
                           ':value (clj-parse--leaf-token-value token))
     stack)))

(defun clj-ast--reduce-node (stack opener-token children)
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
      (t (cons
          (clj-parse--make-node type pos
                                ':children children)
          stack)))))

(defun clj-ast-parse ()
  (clj-parse-reduce #'clj-ast--reduce-leaf #'clj-ast--reduce-node))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Unparser

(defun clj-parse--string-with-delimiters (nodes ld rd)
  (concat ld (mapconcat #'clj-ast-unparse nodes " ") rd))

(defun clj-ast-unparse (node)
  (if (clj-parse--is-leaf? node)
      (alist-get ':form node)
    (let ((subnodes (alist-get ':children node)))
      (cl-case (a-get node ':node-type)
        (:root (clj-parse--string-with-delimiters subnodes "" ""))
        (:list (clj-parse--string-with-delimiters subnodes "(" ")"))
        (:vector (clj-parse--string-with-delimiters subnodes "[" "]"))
        (:set (clj-parse--string-with-delimiters subnodes "#{" "}"))
        (:map (clj-parse--string-with-delimiters subnodes "{" "}"))
        (:tag )
        ))))

(provide 'clj-ast)

;;; clj-ast.el ends here
