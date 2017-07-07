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

(require 'clj-parse)
(require 'ert)

(defmacro clj-parse-eq-test (parse-to-fn test-string expected)
  `(with-temp-buffer
     (insert ,test-string)
     (goto-char 1)
     (should (equal (,parse-to-fn) ,expected))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; To Elisp code, ala edn.el

(ert-deftest clj-parse-to-elisp-simple-list ()
  (clj-parse-eq-test clj-parse-to-elisp
    "(1 2 3)"
    '((1 2 3))))

(ert-deftest clj-parse-to-elisp-empty-list ()
  (clj-parse-eq-test clj-parse-to-elisp
    "()"
    '(())))

(ert-deftest clj-parse-to-elisp-list-size-1 ()
  (clj-parse-eq-test clj-parse-to-elisp
    "(1)"
    '((1))))

(ert-deftest clj-parse-to-elisp-leafs ()
  (clj-parse-eq-test clj-parse-to-elisp
    "(nil true false hello-world)"
    '((nil t nil hello-world))))

(ert-deftest clj-parse-to-elisp-qualified-symbol ()
  (clj-parse-eq-test clj-parse-to-elisp
    "clojure.string/join"
    '(clojure.string/join)))

(ert-deftest clj-parse-to-elisp-nested-lists ()
  (clj-parse-eq-test clj-parse-to-elisp
    "((.9 abc (true) (hello)))"
    '(((0.9 abc (t) (hello))))))

(ert-deftest clj-parse-to-elisp-strings ()
  (clj-parse-eq-test clj-parse-to-elisp
    "\"abc hello \\t\\\"x\""
    '("abc hello \t\"x"))
  (clj-parse-eq-test clj-parse-to-elisp
    "(\"---\\f---\\\"-'\\'-\\\\-\\r\\n\")"
    '(("---\f---\"-''-\\-\r\n"))))

(ert-deftest clj-parse-to-elisp-chars ()
  (clj-parse-eq-test clj-parse-to-elisp
    "(\\newline \\return \\space \\tab \\a \\b \\c \\u0078 \\o171)"
    '((?\n ?\r ?\ ?\t ?a ?b ?c ?x ?y)))
  (clj-parse-eq-test clj-parse-to-elisp
    "\"\\u0078 \\o171\""
    '("x y")))

(ert-deftest clj-parse-to-elisp-keywords ()
  (clj-parse-eq-test clj-parse-to-elisp
    ":foo-bar"
    '(:foo-bar)))

(ert-deftest clj-parse-to-elisp-vector ()
  (clj-parse-eq-test clj-parse-to-elisp
    "[123]"
    '([123])))

(ert-deftest clj-parse-to-elisp-map ()
  (clj-parse-eq-test clj-parse-to-elisp
    "{:count 123}"
    '(((:count . 123)))))

(ert-deftest clj-parse-to-elisp-set ()
  (clj-parse-eq-test clj-parse-to-elisp
    "#{:x}"
    '((:x))))

(ert-deftest clj-parse-to-elisp-discarded ()
  (clj-parse-eq-test clj-parse-to-elisp
    "(10 #_11 12 #_#_ 13 14)"
    '((10 12 14))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; To Clojure/EDN string

(ert-deftest clj-parse-to-string-simple-list ()
  (clj-parse-eq-test clj-parse-to-string
    "(   1 2   3)"
    "(1 2 3)"))

(ert-deftest clj-parse-to-string-empty-list ()
  (clj-parse-eq-test clj-parse-to-string
    "()"
    "()"))

(ert-deftest clj-parse-to-string-list-size-1 ()
  (clj-parse-eq-test clj-parse-to-string
    "(1)"
    "(1)"))

(ert-deftest clj-parse-to-string-leafs ()
  (clj-parse-eq-test clj-parse-to-string
    "(nil true false hello-world)"
    "(nil true false hello-world)"))

(ert-deftest clj-parse-to-string-qualified-symbol ()
  (clj-parse-eq-test clj-parse-to-string
    "clojure.string/join"
    "clojure.string/join"))

(ert-deftest clj-parse-to-string-nested-lists ()
  (clj-parse-eq-test clj-parse-to-string
    "((.9 abc (true) (hello)))"
    "((.9 abc (true) (hello)))"))

(ert-deftest clj-parse-to-string-strings ()
  (clj-parse-eq-test clj-parse-to-string
    "\"abc hello \\t\\\"x\""
    "\"abc hello \\t\\\"x\"")
  (clj-parse-eq-test clj-parse-to-string
    "(\"---\\f---\\\"-'\\'-\\\\-\\r\\n\")"
    "(\"---\\f---\\\"-'\\'-\\\\-\\r\\n\")"))

(ert-deftest clj-parse-to-string-chars ()
  (clj-parse-eq-test clj-parse-to-string
    "(\\newline \\return \\space \\tab \\a \\b \\c \\u0078 \\o171)"
    "(\\newline \\return \\space \\tab \\a \\b \\c \\u0078 \\o171)")
  (clj-parse-eq-test clj-parse-to-string
    "\"\\u0078 \\o171\""
    "\"\\u0078 \\o171\""))

(ert-deftest clj-parse-to-string-keywords ()
  (clj-parse-eq-test clj-parse-to-string
    ":foo-bar"
    ":foo-bar"))

(ert-deftest clj-parse-to-string-vector ()
  (clj-parse-eq-test clj-parse-to-string
    "[123]"
    "[123]"))

(ert-deftest clj-parse-to-string-map ()
  (clj-parse-eq-test clj-parse-to-string
    "{:count 123}"
    "{:count 123}"))

(ert-deftest clj-parse-to-string-set ()
  (clj-parse-eq-test clj-parse-to-string
    "#{:x}"
    "#{:x}"))

(ert-deftest clj-parse-to-string-discarded ()
  (clj-parse-eq-test clj-parse-to-string
    "(10 #_11 12 #_#_ 13 14)"
    "(10 12 14)"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AST

(ert-deftest clj-parse-ast-simple-list ()
  (clj-parse-eq-test clj-parse
    "(1 2 3)"
    '((type . :root)
      (subnodes . (((type . :list)
                    (subnodes . (((type . :number) (form . "1") (pos . 2))
                                 ((type . :number) (form . "2") (pos . 4))
                                 ((type . :number) (form . "3") (pos . 6))))))))))

(provide 'clj-parse-test)

;;; clj-parse-test.el ends here
