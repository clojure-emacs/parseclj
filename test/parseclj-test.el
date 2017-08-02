;;; parseclj-test.el --- Clojure/EDN parser - tests

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
(require 'parseclj)

(ert-deftest parseclj-parse-clojure-with-lexical-preservation-test ()
  (should (equal
           (parseclj-parse-clojure ";; foo\nbar")
           '((:node-type . :root)
             (:position . 1)
             (:children ((:node-type . :symbol)
                         (:position . 8)
                         (:form . "bar")
                         (:value . bar))))))
  (should (equal
           (parseclj-parse-clojure ";; foo\nbar" :lexical-preservation t)
           '((:node-type . :root)
             (:lexical-preservation . t)
             (:position . 1)
             (:children ((:node-type . :comment)
                         (:position . 1)
                         (:form . ";; foo\n"))
                        ((:node-type . :symbol)
                         (:position . 8)
                         (:form . "bar")
                         (:value . bar))))))
  (should (equal
           (parseclj-parse-clojure ";; foo\n;;baz\nbar" :lexical-preservation t)
           '((:node-type . :root)
             (:lexical-preservation . t)
             (:position . 1)
             (:children ((:node-type . :comment)
                         (:position . 1)
                         (:form . ";; foo\n;;baz\n"))
                        ((:node-type . :symbol)
                         (:position . 14)
                         (:form . "bar")
                         (:value . bar)))))))

(ert-deftest parseclj-parse-clojure-fail-fast-test ()
  (should (equal
           (condition-case errdata
               (parseclj-parse-clojure "foo]")
             (parseclj-parse-error (cadr errdata)))
           "parseclj: Syntax Error at position 4, unmatched :rbracket"))

  (should (equal
           (condition-case errdata
               (parseclj-parse-clojure "[foo")
             (parseclj-parse-error (cadr errdata)))
           "parseclj: Syntax Error at position 1, unmatched :lbracket"))

  (should (equal
           (condition-case errdata
               (parseclj-parse-clojure "(1 2 [ 4)")
             (parseclj-parse-error (cadr errdata)))
           "parseclj: Syntax Error at position 6, unmatched :lbracket"))

  (should (equal
           (condition-case errdata
               (parseclj-parse-clojure "1 2 #_")
             (parseclj-parse-error (cadr errdata)))
           "parseclj: Syntax Error at position 5, unmatched :discard"))

  (should (equal
           (condition-case errdata
               (parseclj-parse-clojure "(1 [2 {3 ( 4}])")
             (parseclj-parse-error (cadr errdata)))
           "parseclj: Syntax Error at position 10, unmatched :lparen")))

(ert-deftest parseclj-parse-clojure-fail-fast-test ()
  (should (equal
           (condition-case errdata
               (parseclj-parse-clojure "foo]")
             (parseclj-parse-error (cadr errdata)))
           "parseclj: Syntax Error at position 4, unmatched :rbracket"))

  (should (equal
           (condition-case errdata
               (parseclj-parse-clojure "[foo")
             (parseclj-parse-error (cadr errdata)))
           "parseclj: Syntax Error at position 1, unmatched :lbracket"))

  (should (equal
           (condition-case errdata
               (parseclj-parse-clojure "(1 2 [ 4)")
             (parseclj-parse-error (cadr errdata)))
           "parseclj: Syntax Error at position 6, unmatched :lbracket"))

  (should (equal
           (condition-case errdata
               (parseclj-parse-clojure "1 2 #_")
             (parseclj-parse-error (cadr errdata)))
           "parseclj: Syntax Error at position 5, unmatched :discard"))

  (should (equal (parseclj-parse-clojure "(1 [2 {3 ( 4}])" :fail-fast nil)
                 '((:node-type . :root)
                   (:position . 1)
                   (:children ((:node-type . :list)
                               (:position . 1)
                               (:children ((:node-type . :number)
                                           (:position . 2)
                                           (:form . "1")
                                           (:value . 1))
                                          ((:node-type . :vector)
                                           (:position . 4)
                                           (:children ((:node-type . :number)
                                                       (:position . 5)
                                                       (:form . "2")
                                                       (:value . 2))
                                                      ((:node-type . :map)
                                                       (:position . 7)
                                                       (:children ((:node-type . :number) (:position . 8) (:form . "3") (:value . 3))
                                                                  ((:token-type . :lparen) (:form . "(") (:pos . 10))
                                                                  ((:node-type . :number) (:position . 12) (:form . "4") (:value . 4))))))))))))

  ;; TODO: uneven map forms
  )

(provide 'parseclj-test)

;;; parseclj-test.el ends here
