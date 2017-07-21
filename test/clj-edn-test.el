;;; clj-edn-test.el --- Unit tests for EDN reading/printing

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

;; Unit tests for EDN reading/printing

;;; Code

(require 'ert)
(require 'clj-parse)

(load "test/clj-parse-test-data.el")

(ert-deftest clj-edn-print-test ()
  (should (equal (clj-edn-print-str nil) "nil"))
  (should (equal (clj-edn-print-str 100) "100"))
  (should (equal (clj-edn-print-str 1.2) "1.2"))
  (should (equal (clj-edn-print-str [1 2 3]) "[1 2 3]"))
  (should (equal (clj-edn-print-str t) "true")))

(ert-deftest clj-edn-read-test ()
  (should (equal (clj-edn-read-str "true") t)))

(defmacro define-clj-edn-read-tests ()
  `(progn
     ,@(mapcar
        (lambda (pair)
          (let ((name (car pair))
                (data (cdr pair)))
            (if (and (a-get data :edn) (a-get data :source))
                (let ((test-name (intern (concat "clj-edn-read:" name))))
                  `(ert-deftest ,test-name ()
                     :tags '(clj-edn)
                     (with-temp-buffer
                       (insert ,(a-get data :source))
                       (goto-char 1)
                       (should (a-equal (clj-edn-read) ',(a-get data :edn)))))))))
        clj-parse-test-data)))

(defmacro define-clj-edn-roundtrip-tests ()
  `(progn
     ,@(mapcar
        (lambda (pair)
          (let ((name (car pair))
                (data (cdr pair)))
            (if (and (a-get data :edn) (a-get data :source) (member :edn-roundtrip (a-get data :tags)))
                (let ((test-name (intern (concat "clj-edn-rountrip:" name))))
                  `(ert-deftest ,test-name ()
                     :tags '(clj-edn-rountrip)
                     (should (equal (clj-edn-print-str (car ',(a-get data :edn))) ,(a-get data :source))))))))
        clj-parse-test-data)))

(define-clj-edn-read-tests)
(define-clj-edn-roundtrip-tests)

;;; clj-edn-test.el
