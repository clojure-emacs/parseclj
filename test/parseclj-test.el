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
             (parseclj-parser-error (cadr errdata)))
           "At position 4, unmatched :rbracket"))

  (should (equal
           (condition-case errdata
               (parseclj-parse-clojure "[foo")
             (parseclj-parser-error (cadr errdata)))
           "At position 1, unmatched :lbracket"))

  (should (equal
           (condition-case errdata
               (parseclj-parse-clojure "(1 2 [ 4)")
             (parseclj-parser-error (cadr errdata)))
           "At position 6, unmatched :lbracket"))

  (should (equal
           (condition-case errdata
               (parseclj-parse-clojure "1 2 #_")
             (parseclj-parser-error (cadr errdata)))
           "At position 5, unmatched :discard"))

  (should (equal
           (condition-case errdata
               (parseclj-parse-clojure "(1 [2 {3 ( 4}])")
             (parseclj-parser-error (cadr errdata)))
           "At position 10, unmatched :lparen")))

(ert-deftest parseclj-parse-clojure-not-fail-fast-test ()
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

(ert-deftest parseclj-parse-clojure-lexical-preservation ()
  (should (equal
           (parseclj-parse-clojure "#_ (1 2 3) true")
           '((:node-type . :root) (:position . 1) (:children ((:node-type . :true) (:position . 12) (:form . "true") (:value . t))))))
  (should (equal
           (parseclj-parse-clojure "#_(1 2 3) true" :lexical-preservation t)
           '((:node-type . :root)
             (:lexical-preservation . t)
             (:position . 1)
             (:children ((:node-type . :discard)
                         (:position . 1)
                         (:children ((:node-type . :list)
                                     (:lexical-preservation . t)
                                     (:position . 3)
                                     (:children ((:node-type . :number) (:position . 4) (:form . "1") (:value . 1))
                                                ((:node-type . :whitespace) (:position . 5) (:form . " "))
                                                ((:node-type . :number) (:position . 6) (:form . "2") (:value . 2))
                                                ((:node-type . :whitespace) (:position . 7) (:form . " "))
                                                ((:node-type . :number) (:position . 8) (:form . "3") (:value . 3))))))
                        ((:node-type . :whitespace)
                         (:position . 10)
                         (:form . " "))
                        ((:node-type . :true)
                         (:position . 11)
                         (:form . "true")
                         (:value . t))))))

  (should (equal
           (parseclj-parse-clojure "#_ (1 2 3) true" :lexical-preservation t)
           '((:node-type . :root)
             (:lexical-preservation . t)
             (:position . 1)
             (:children ((:node-type . :discard)
                         (:position . 1)
                         (:children
                          ((:node-type . :whitespace) (:position . 3) (:form . " "))
                          ((:node-type . :list)
                           (:lexical-preservation . t)
                           (:position . 4)
                           (:children ((:node-type . :number) (:position . 5) (:form . "1") (:value . 1))
                                      ((:node-type . :whitespace) (:position . 6) (:form . " "))
                                      ((:node-type . :number) (:position . 7) (:form . "2") (:value . 2))
                                      ((:node-type . :whitespace) (:position . 8) (:form . " "))
                                      ((:node-type . :number) (:position . 9) (:form . "3") (:value . 3))))))
                        ((:node-type . :whitespace)
                         (:position . 11)
                         (:form . " "))
                        ((:node-type . :true)
                         (:position . 12)
                         (:form . "true")
                         (:value . t))))))

  (should (equal
           (parseclj-parse-clojure "#_#_4 5" :lexical-preservation t)
           '((:node-type . :root)
             (:lexical-preservation . t)
             (:position . 1)
             (:children ((:node-type . :discard)
                         (:position . 1)
                         (:children ((:node-type . :discard)
                                     (:position . 3)
                                     (:children ((:node-type . :number) (:position . 5) (:form . "4") (:value . 4))))
                                    ((:node-type . :whitespace) (:position . 6) (:form . " "))
                                    ((:node-type . :number) (:position . 7) (:form . "5") (:value . 5)))))))))

(ert-deftest parseclj--take-token-test ()
  (should (equal
           (parseclj--take-token
            (list (parseclj-ast-node :whitespace 10)
                  (parseclj-ast-node :comment 20)
                  (parseclj-lex-token :discard "#_" 30)
                  (parseclj-ast-node :comment 20))
            (lambda (e)
              (and (parseclj-ast-node-p e)
                   (not (member (parseclj-ast-node-type e) '(:whitespace :comment :discard)))))
            '(:discard))
           '(((:token-type . :discard) (:form . "#_") (:pos . 30))
             ((:node-type . :comment) (:position . 20))
             ((:node-type . :whitespace) (:position . 10)))))

  (should (equal
           (parseclj--take-token
            (list (parseclj-ast-node :whitespace 10)
                  (parseclj-ast-node :number 20)
                  (parseclj-lex-token :discard "#_" 30)
                  (parseclj-ast-node :comment 20))
            (lambda (e)
              (and (parseclj-ast-node-p e)
                   (not (member (parseclj-ast-node-type e) '(:whitespace :comment :discard)))))
            '(:discard))
           nil)))

(ert-deftest parseclj--take-value-test ()
  (let ((stack '(((:node-type . :number) (:position . 3) (:form . "4") (:value . 4))
                 ((:token-type . :discard) (:form . "#_") (:pos . 1))))
        (value-p (lambda (e)
                   (and (parseclj-ast-node-p e)
                        (not (member (parseclj-ast-node-type e) '(:whitespace :comment :discard)))))))
    (should (equal (parseclj--take-value stack value-p)
                   '(((:node-type . :number) (:position . 3) (:form . "4") (:value . 4)))))

    (let* ((top-value (parseclj--take-value stack value-p))
           (opening-token (parseclj--take-token (nthcdr (length top-value) stack) value-p '(:discard :tag)))
           (new-stack (nthcdr (+ (length top-value) (length opening-token)) stack)))

      (should (equal top-value '(((:node-type . :number) (:position . 3) (:form . "4") (:value . 4)))))
      (should (equal opening-token '(((:token-type . :discard) (:form . "#_") (:pos . 1)))))
      (should (equal new-stack nil))))

  (let ((stack '(((:node-type . :whitespace) (:position . 3) (:form . " "))
                 ((:token-type . :discard) (:form . "#_") (:pos . 1))))
        (value-p (lambda (e)
                   (and (parseclj-ast-node-p e)
                        (not (member (parseclj-ast-node-type e) '(:whitespace :comment :discard)))))))

    (let* ((top-value (parseclj--take-value stack value-p))
           (opening-token (parseclj--take-token (nthcdr (length top-value) stack) value-p '(:discard :tag)))
           (new-stack (nthcdr (+ (length top-value) (length opening-token)) stack)))
      top-value)))

(provide 'parseclj-test)

;;; parseclj-test.el ends here
