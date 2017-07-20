;;; clj-ast-test.el --- Unit tests for AST parsing/unparsing

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

;;; Commentary

;; Unit tests for AST parsing/unparsing

;;; Code

(require 'ert)
(require 'clj-ast)

(load "test/clj-parse-test-data.el")

(defmacro define-clj-ast-parse-tests ()
  `(progn
     ,@(mapcar
        (lambda (pair)
          (let ((name (car pair))
                (data (cdr pair)))
            (if (and (a-get data :source) (a-get data :ast))
                (let ((test-name (intern (concat "clj-ast-parse:" name))))
                  `(ert-deftest ,test-name ()
                     :tags '(clj-ast)
                     (with-temp-buffer
                       (insert ,(a-get data :source))
                       (goto-char 1)
                       (should (a-equal (clj-ast-parse) ',(a-get data :ast)))))))))
        clj-parse-test-data)))

(defmacro define-clj-ast-roundtrip-tests ()
  `(progn
     ,@(mapcar
        (lambda (pair)
          (let ((name (car pair))
                (data (cdr pair)))
            (if (and (a-get data :ast) (a-get data :source))
                (let ((test-name (intern (concat "clj-ast-rountrip:" name))))
                  `(ert-deftest ,test-name ()
                     :tags '(clj-ast-rountrip)
                     (should (a-equal (clj-ast-parse-str (clj-ast-unparse-str ',(a-get data :ast))) ',(a-get data :ast))))))))
        clj-parse-test-data)))


(define-clj-ast-roundtrip-tests)
(define-clj-ast-parse-tests)

;;; clj-ast-test.el ends here
