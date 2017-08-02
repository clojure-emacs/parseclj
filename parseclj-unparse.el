;;; parseclj-unparser.el --- Clojure unparser   -*- lexical-binding: t; -*-

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

;; Unparse an AST to Clojure code

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Unparser helpers

(defun parseclj-unparse--collection (node ld rd)
  (insert ld)
  (let ((nodes (alist-get ':children node)))
    (when-let (node (car nodes))
      (parseclj-unparse-clojure node))
    (seq-doseq (child (cdr nodes))
      (when (not (a-get node :lexical-preservation))
        (insert " "))
      (parseclj-unparse-clojure child)))
  (insert rd))

(defun parseclj-unparse--tag (node)
  (progn
    (insert "#")
    (insert (symbol-name (a-get node :tag)))
    (insert " ")
    (parseclj-unparse-clojure (car (a-get node :children)))))

(provide 'parseclj-unparse)

;;; parseclj-unparse.el ends here
