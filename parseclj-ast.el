;;; parseclj-ast.el --- Clojure parser/unparser              -*- lexical-binding: t; -*-

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

;; AST helper functions

(defun parseclj-ast-node (type position &rest attributes)
  "Create an AST node with given TYPE and POSITION.

Other ATTRIBUTES can be given as a flat list of key-value pairs. "
  (apply 'a-list :node-type type :position position attributes))

(defun parseclj-ast-node? (node)
  "Return `t' if the given NODE is a Clojure AST node."
  (and (consp node)
       (consp (car node))
       (eq :node-type (caar node))))

(defun parseclj-ast-node-type (node)
  "Return the type of the AST node NODE."
  (a-get node :node-type))

(defun parseclj-ast-leaf-node? (node)
  "Return `t' if the given ast NODE is a leaf node."
  (member (parseclj-ast-node-type node) parseclj--leaf-tokens))

;; Parse/reduce strategy functions

(defun parseclj-ast--reduce-leaf (stack token)
  (if (member (parseclj-lex-token-type token) '(:whitespace :comment))
      stack
    (cons
     (parseclj-ast-node (parseclj-lex-token-type token)
                        (a-get token :pos)
                        :form (a-get token :form)
                        :value (parseclj--leaf-token-value token))
     stack)))

(defun parseclj-ast--reduce-leaf-with-lexical-preservation (stack token)
  (let ((token-type (parseclj-lex-token-type token))
        (top (car stack)))
    (if (member token-type '(:whitespace :comment))
        ;; merge consecutive whitespace or comment tokens
        (if (eq token-type (a-get top :node-type))
            (cons (a-update top :form #'concat (a-get token :form))
                  (cdr stack))
          (cons (parseclj-ast-node (parseclj-lex-token-type token)
                                   (a-get token :pos)
                                   :form (a-get token :form))
                stack))
      (parseclj-ast--reduce-leaf stack token))))

(defun parseclj-ast--reduce-branch (stack opening-token children)
  (let* ((pos (a-get opening-token :pos))
         (type (parseclj-lex-token-type opening-token))
         (type (cl-case type
                 (:lparen :list)
                 (:lbracket :vector)
                 (:lbrace :map)
                 (t type))))
    (cl-case type
      (:root (cons (parseclj-ast-node :root pos :children children) stack))
      (:discard stack)
      (:tag (list (parseclj-ast-node :tag
                                     pos
                                     :tag (intern (substring (a-get opening-token :form) 1))
                                     :children children)))
      (t (cons
          (parseclj-ast-node type pos :children children)
          stack)))))

(defun parseclj-ast--reduce-branch-with-lexical-preservation (&rest args)
  (let* ((stack (apply #'parseclj-ast--reduce-branch args))
         (top (car stack)))
    (if (parseclj-ast-node? top)
        (cons (cl-list* (car top) ;; make sure :node-type remains the first element in the list
                        '(:lexical-preservation . t)
                        (cdr top))
              (cdr stack))
      stack)))

(provide 'parseclj-ast)

;;; parseclj-ast.el ends here
