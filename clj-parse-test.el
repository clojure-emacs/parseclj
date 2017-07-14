;;; clj-parse-test.el --- Clojure/EDN parser

;; Copyright (C) 2017  Arne Brasseur

;; Author: Arne Brasseur <arne@arnebrasseur.net>
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

(require 'a)
(require 'ert)
(require 'clj-parse)

(defun clj-parse--equal (a b)
  (cond
   ((and (hash-table-p a) (hash-table-p b))
    (a-equal a b))
   ((and (consp a) (consp b))
    (and (clj-parse--equal (car a) (car b))
         (clj-parse--equal (cdr a) (cdr b))))
   (t (equal a b))))

(defun clj-parse--deftest-mode (mode test-name test-string expected)
  (let* ((parse-fn (intern (concat "clj-parse-" mode)))
         (test-name (intern (concat (symbol-name parse-fn) "-" (symbol-name test-name)))))
    `(ert-deftest ,test-name ()
       (with-temp-buffer
         (insert ,test-string)
         (goto-char 1)
         (should (clj-parse--equal (,parse-fn) ,expected))))))

(defmacro clj-parse-deftest (test-name test-string mode-vs-expected-alist)
  (declare (indent defun))
  `(progn
     ,@(mapcar (lambda (vs) (clj-parse--deftest-mode (car vs)
                                                     test-name
                                                     test-string
                                                     (cadr vs)))
               mode-vs-expected-alist)))


;;; Parser modes
;; ----------------------------------------------------------------------------

(clj-parse-deftest simple-list "(1 2 3)"
  (("edn" '((1 2 3)))
   ("ast" '((:node-type . :root)
            (:position . 0)
            (:children . (((:node-type . :list)
                           (:position . 1)
                           (:children . (((:node-type . :number)
                                          (:position . 2)
                                          (:form . "1")
                                          (:value . 1))
                                         ((:node-type . :number)
                                          (:position . 4)
                                          (:form . "2")
                                          (:value . 2))
                                         ((:node-type . :number)
                                          (:position . 6)
                                          (:form . "3")
                                          (:value . 3)))))))))))


(clj-parse-deftest empty-list "()"
  (("edn" '(()))
   ("ast" '((:node-type . :root)
            (:position . 0)
            (:children . (((:node-type . :list)
                           (:position . 1)
                           (:children . nil))))))))

(clj-parse-deftest size-1 "(1)"
  (("edn" '((1)))
   ("ast" '((:node-type . :root)
            (:position . 0)
            (:children . (((:node-type . :list)
                           (:position . 1)
                           (:children . (((:node-type . :number)
                                          (:position . 2)
                                          (:form . "1")
                                          (:value . 1)))))))))))

(clj-parse-deftest leafs "(nil true false hello-world)"
  (("edn" '((nil t nil hello-world)))
   ("ast" '((:node-type . :root)
            (:position . 0)
            (:children . (((:node-type . :list)
                           (:position . 1)
                           (:children . (((:node-type . :nil)
                                          (:position . 2)
                                          (:form . "nil")
                                          (:value . nil))
                                         ((:node-type . :true)
                                          (:position . 6)
                                          (:form . "true")
                                          (:value . t))
                                         ((:node-type . :false)
                                          (:position . 11)
                                          (:form . "false")
                                          (:value . nil))
                                         ((:node-type . :symbol)
                                          (:position . 17)
                                          (:form . "hello-world")
                                          (:value . hello-world)))))))))))

(clj-parse-deftest qualified-symbol "clojure.string/join"
  (("edn" '(clojure.string/join))
   ("ast" '((:node-type . :root)
            (:position . 0)
            (:children . (((:node-type . :symbol)
                           (:position . 1)
                           (:form . "clojure.string/join")
                           (:value . clojure.string/join))))))))

(clj-parse-deftest nested-lists "((.9 abc (true) (hello)))"
  (("edn" '(((0.9 abc (t) (hello)))))
   ("ast" '((:node-type . :root)
            (:position . 0)
            (:children . (((:node-type . :list)
                           (:position . 1)
                           (:children . (((:node-type . :list)
                                          (:position . 2)
                                          (:children ((:node-type . :number)
                                                      (:position . 3)
                                                      (:form . ".9")
                                                      (:value . 0.9))
                                                     ((:node-type . :symbol)
                                                      (:position . 6)
                                                      (:form . "abc")
                                                      (:value . abc))
                                                     ((:node-type . :list)
                                                      (:position . 10)
                                                      (:children ((:node-type . :true)
                                                                  (:position . 11)
                                                                  (:form . "true")
                                                                  (:value . t))))
                                                     ((:node-type . :list)
                                                      (:position . 17)
                                                      (:children ((:node-type . :symbol)
                                                                  (:position . 18)
                                                                  (:form . "hello")
                                                                  (:value . hello)))))))))))))))

(clj-parse-deftest strings-1 "\"abc hello \\t\\\"x\""
  (("edn" '("abc hello \t\"x"))
   ("ast" '((:node-type . :root)
            (:position . 0)
            (:children . (((:node-type . :string)
                           (:position . 1)
                           (:form . "\"abc hello \\t\\\"x\"")
                           (:value . "abc hello \t\"x"))))))))

(clj-parse-deftest strings-2 "(\"---\\f---\\\"-'\\'-\\\\-\\r\\n\")"
  (("edn" '(("---\f---\"-''-\\-\r\n")))
   ("ast" '((:node-type . :root)
            (:position . 0)
            (:children . (((:node-type . :list)
                           (:position . 1)
                           (:children . (((:node-type . :string)
                                          (:position . 2)
                                          (:form . "\"---\\f---\\\"-'\\'-\\\\-\\r\\n\"")
                                          (:value . "---\f---\"-''-\\-\r\n")))))))))))

(clj-parse-deftest chars-1 "(\\newline \\return \\space \\tab \\a \\b \\c \\u0078 \\o171)"
  (("edn" '((?\n ?\r ?\ ?\t ?a ?b ?c ?x ?y)))
   ("ast" '((:node-type . :root)
            (:position . 0)
            (:children . (((:node-type . :list)
                           (:position . 1)
                           (:children . (((:node-type . :character) (:position . 2) (:form . "\\newline") (:value . ?\n))
                                         ((:node-type . :character) (:position . 11) (:form . "\\return") (:value . ?\r))
                                         ((:node-type . :character) (:position . 19) (:form . "\\space") (:value . 32))
                                         ((:node-type . :character) (:position . 26) (:form . "\\tab") (:value . ?\t))
                                         ((:node-type . :character) (:position . 31) (:form . "\\a") (:value . ?a))
                                         ((:node-type . :character) (:position . 34) (:form . "\\b") (:value . ?b))
                                         ((:node-type . :character) (:position . 37) (:form . "\\c") (:value . ?c))
                                         ((:node-type . :character) (:position . 40) (:form . "\\u0078") (:value . ?x))
                                         ((:node-type . :character) (:position . 47) (:form . "\\o171") (:value . ?y)))))))))))

(clj-parse-deftest chars-2 "\"\\u0078 \\o171\""
  (("edn" '("x y"))
   ("ast" '((:node-type . :root)
            (:position . 0)
            (:children . (((:node-type . :string)
                           (:position . 1)
                           (:form . "\"\\u0078 \\o171\"")
                           (:value . "x y"))))))))

(clj-parse-deftest keywords ":foo-bar"
  (("edn" '(:foo-bar))
   ("ast" '((:node-type . :root)
            (:position . 0)
            (:children . (((:node-type . :keyword)
                           (:position . 1)
                           (:form . ":foo-bar")
                           (:value . :foo-bar))))))))

(clj-parse-deftest vector "[123]"
  (("edn" '([123]))
   ("ast" '((:node-type . :root)
            (:position . 0)
            (:children . (((:node-type . :vector)
                           (:position . 1)
                           (:children . (((:node-type . :number)
                                          (:position . 2)
                                          (:form . "123")
                                          (:value . 123)))))))))))

(clj-parse-deftest map "{:count 123}"
  (("edn" (list (a-hash-table :count 123)))
   ("ast" '((:node-type . :root)
            (:position . 0)
            (:children . (((:node-type . :map)
                           (:position . 1)
                           (:children . (((:node-type . :keyword)
                                          (:position . 2)
                                          (:form . ":count")
                                          (:value . :count))
                                         ((:node-type . :number)
                                          (:position . 9)
                                          (:form . "123")
                                          (:value . 123)))))))))))

(clj-parse-deftest set "#{:x}"
  (("edn" '((edn-set (:x))))
   ("ast" '((:node-type . :root)
            (:position . 0)
            (:children . (((:node-type . :set)
                           (:position . 1)
                           (:children . (((:node-type . :keyword)
                                          (:position . 3)
                                          (:form . ":x")
                                          (:value . :x)))))))))))

(clj-parse-deftest discard "(10 #_11 12 #_#_ 13 14)"
  (("edn" '((10 12)))
   ("ast" '((:node-type . :root)
            (:position . 0)
            (:children . (((:node-type . :list)
                           (:position . 1)
                           (:children . (((:node-type . :number)
                                          (:position . 2)
                                          (:form . "10")
                                          (:value . 10))
                                         ((:node-type . :number)
                                          (:position . 10)
                                          (:form . "12")
                                          (:value . 12)))))))))))


;;; Printer modes
;; ----------------------------------------------------------------------------

(ert-deftest clj-parse-ast-print-list ()
  (should (equal "(0 1 2)"
                 (clj-parse-ast-print '((:node-type . :root)
                                        (:position . 0)
                                        (:children . (((:node-type . :list)
                                                       (:position . 1)
                                                       (:children . (((:node-type . :number)
                                                                      (:position . 2)
                                                                      (:form . "0")
                                                                      (:value . 0))
                                                                     ((:node-type . :number)
                                                                      (:position . 4)
                                                                      (:form . "1")
                                                                      (:value . 1))
                                                                     ((:node-type . :number)
                                                                      (:position . 6)
                                                                      (:form . "2")
                                                                      (:value . 2))))))))))))

(ert-deftest clj-parse-ast-print-empty-list ()
  (should (equal "()"
                 (clj-parse-ast-print '((:node-type . :root)
                                        (:position . 0)
                                        (:children . (((:node-type . :list)
                                                       (:position . 1)
                                                       (:children . nil)))))))))

(ert-deftest clj-parse-ast-print-nested-list ()
  (should (equal "((.9 abc (true) (hello)))"
                 (clj-parse-ast-print '((:node-type . :root)
                                        (:position . 0)
                                        (:children . (((:node-type . :list)
                                                       (:position . 1)
                                                       (:children . (((:node-type . :list)
                                                                      (:position . 2)
                                                                      (:children ((:node-type . :number)
                                                                                  (:position . 3)
                                                                                  (:form . ".9")
                                                                                  (:value . 0.9))
                                                                                 ((:node-type . :symbol)
                                                                                  (:position . 6)
                                                                                  (:form . "abc")
                                                                                  (:value . abc))
                                                                                 ((:node-type . :list)
                                                                                  (:position . 10)
                                                                                  (:children ((:node-type . :true)
                                                                                              (:position . 11)
                                                                                              (:form . "true")
                                                                                              (:value . t))))
                                                                                 ((:node-type . :list)
                                                                                  (:position . 17)
                                                                                  (:children ((:node-type . :symbol)
                                                                                              (:position . 18)
                                                                                              (:form . "hello")
                                                                                              (:value . hello))))))))))))))))

(ert-deftest clj-parse-ast-print-string ()
  (should (equal "\"abc hello \\t\\\"x\""
                 (clj-parse-ast-print '((:node-type . :root)
                                        (:position . 0)
                                        (:children . (((:node-type . :string)
                                                       (:position . 1)
                                                       (:form . "\"abc hello \\t\\\"x\"")
                                                       (:value . "abc hello \t\"x")))))))))

(ert-deftest clj-parse-ast-print-chars ()
  (should (equal "(\\newline \\return \\space \\tab \\a \\b \\c \\u0078 \\o171)"
                 (clj-parse-ast-print '((:node-type . :root)
            (:position . 0)
            (:children . (((:node-type . :list)
                           (:position . 1)
                           (:children . (((:node-type . :character) (:position . 2) (:form . "\\newline") (:value . ?\n))
                                         ((:node-type . :character) (:position . 11) (:form . "\\return") (:value . ?\r))
                                         ((:node-type . :character) (:position . 19) (:form . "\\space") (:value . 32))
                                         ((:node-type . :character) (:position . 26) (:form . "\\tab") (:value . ?\t))
                                         ((:node-type . :character) (:position . 31) (:form . "\\a") (:value . ?a))
                                         ((:node-type . :character) (:position . 34) (:form . "\\b") (:value . ?b))
                                         ((:node-type . :character) (:position . 37) (:form . "\\c") (:value . ?c))
                                         ((:node-type . :character) (:position . 40) (:form . "\\u0078") (:value . ?x))
                                         ((:node-type . :character) (:position . 47) (:form . "\\o171") (:value . ?y))))))))))))

(ert-deftest clj-parse-ast-print-keyword ()
  (should (equal ":foo-bar"
                 (clj-parse-ast-print '((:node-type . :root)
                                        (:position . 0)
                                        (:children . (((:node-type . :keyword)
                                                       (:position . 1)
                                                       (:form . ":foo-bar")
                                                       (:value . :foo-bar)))))))))

(ert-deftest clj-parse-ast-print-vector ()
  (should (equal "[123]"
                 (clj-parse-ast-print '((:node-type . :root)
                                        (:position . 0)
                                        (:children . (((:node-type . :vector)
                                                       (:position . 1)
                                                       (:children . (((:node-type . :number)
                                                                      (:position . 2)
                                                                      (:form . "123")
                                                                      (:value . 123))))))))))))

(ert-deftest clj-parse-ast-print-map ()
  (should (equal "{:count 123}"
                 (clj-parse-ast-print '((:node-type . :root)
                                        (:position . 0)
                                        (:children . (((:node-type . :map)
                                                       (:position . 1)
                                                       (:children . (((:node-type . :keyword)
                                                                      (:position . 2)
                                                                      (:form . ":count")
                                                                      (:value . :count))
                                                                     ((:node-type . :number)
                                                                      (:position . 9)
                                                                      (:form . "123")
                                                                      (:value . 123))))))))))))

(ert-deftest clj-parse-ast-print-set ()
  (should (equal "#{:x}"
                 (clj-parse-ast-print '((:node-type . :root)
                                        (:position . 0)
                                        (:children . (((:node-type . :set)
                                                       (:position . 1)
                                                       (:children . (((:node-type . :keyword)
                                                                      (:position . 3)
                                                                      (:form . ":x")
                                                                      (:value . :x))))))))))))

(provide 'clj-parse-test)

;;; clj-parse-test.el ends here
