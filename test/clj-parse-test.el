;;; clj-parse-test.el --- Clojure/EDN parser - tests

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

;; A reader for EDN data files and parser for Clojure source files - tests

;;; Code:

(require 'ert)
(require 'clj-parse)

(defun clj-parse--deftest-mode (mode test-name test-string expected)
  (let* ((parse-fn (if (equal mode "edn")
                       #'clj-edn-read
                     #'clj-ast-parse))
         (test-name (intern (concat (symbol-name parse-fn) "-" (symbol-name test-name)))))
    `(ert-deftest ,test-name ()
       (with-temp-buffer
         (insert ,test-string)
         (goto-char 1)
         (should (a-equal (,parse-fn) (backquote ,expected)))))))

(defmacro clj-parse-deftest (test-name test-string mode-vs-expected-alist)
  (declare (indent defun))
  `(progn
     ,@(let ((edn (a-get mode-vs-expected-alist "edn")))
         (when (eq (length edn) 1)
           `((ert-deftest ,(intern (concat "edn-print-" (symbol-name test-name))) ()
               (should (equal (clj-edn-print-str (backquote ,(car edn))) ,test-string))))))
     ,@(mapcar (lambda (vs) (clj-parse--deftest-mode (car vs)
                                                     test-name
                                                     test-string
                                                     (cdr vs)))
               mode-vs-expected-alist)))


;;; Parser modes
;; ----------------------------------------------------------------------------

(provide 'clj-parse-test)

;;; clj-parse-test.el ends here
