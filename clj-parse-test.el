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

(defmacro clj-parse-deftest (name parse-to-fn test-string expected)
  (declare (indent defun))
  `(ert-deftest ,name ()
     (with-temp-buffer
       (insert ,test-string)
       (goto-char 1)
       (should (equal (,parse-to-fn) ,expected)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; To Elisp code, ala edn.el

(clj-parse-deftest clj-parse-to-elisp-simple-list
  clj-parse-to-elisp
  "(1 2 3)"
  '((1 2 3)))

(clj-parse-deftest clj-parse-to-elisp-empty-list
  clj-parse-to-elisp
  "()"
  '(()))

(clj-parse-deftest clj-parse-to-elisp-list-size-1
  clj-parse-to-elisp
  "(1)"
  '((1)))

(clj-parse-deftest clj-parse-to-elisp-leafs
  clj-parse-to-elisp
  "(nil true false hello-world)"
  '((nil t nil hello-world)))

(clj-parse-deftest clj-parse-to-elisp-qualified-symbol
  clj-parse-to-elisp
  "clojure.string/join"
  '(clojure.string/join))

(clj-parse-deftest clj-parse-to-elisp-nested-lists
  clj-parse-to-elisp
  "((.9 abc (true) (hello)))"
  '(((0.9 abc (t) (hello)))))

(clj-parse-deftest clj-parse-to-elisp-strings-1
  clj-parse-to-elisp
  "\"abc hello \\t\\\"x\""
  '("abc hello \t\"x"))

(clj-parse-deftest clj-parse-to-elisp-strings-2
  clj-parse-to-elisp
  "(\"---\\f---\\\"-'\\'-\\\\-\\r\\n\")"
  '(("---\f---\"-''-\\-\r\n")))

(clj-parse-deftest clj-parse-to-elisp-chars-1
  clj-parse-to-elisp
  "(\\newline \\return \\space \\tab \\a \\b \\c \\u0078 \\o171)"
  '((?\n ?\r ?\ ?\t ?a ?b ?c ?x ?y)))

(clj-parse-deftest clj-parse-to-elisp-chars-2
  clj-parse-to-elisp
  "\"\\u0078 \\o171\""
  '("x y"))

(clj-parse-deftest clj-parse-to-elisp-keywords
  clj-parse-to-elisp
  ":foo-bar"
  '(:foo-bar))

(clj-parse-deftest clj-parse-to-elisp-vector
  clj-parse-to-elisp
  "[123]"
  '([123]))

(clj-parse-deftest clj-parse-to-elisp-map
  clj-parse-to-elisp
  "{:count 123}"
  '(((:count . 123))))

(clj-parse-deftest clj-parse-to-elisp-set
  clj-parse-to-elisp
  "#{:x}"
  '((:x)))

(clj-parse-deftest clj-parse-to-elisp-discarded
  clj-parse-to-elisp
  "(10 #_11 12 #_#_ 13 14)"
  '((10 12 14)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; To Clojure/EDN string

(clj-parse-deftest clj-parse-to-string-simple-list
  clj-parse-to-string
  "(   1 2   3)"
  "(1 2 3)")

(clj-parse-deftest clj-parse-to-string-empty-list
  clj-parse-to-string
  "()"
  "()")

(clj-parse-deftest clj-parse-to-string-list-size-1
  clj-parse-to-string
  "(1)"
  "(1)")

(clj-parse-deftest clj-parse-to-string-leafs
  clj-parse-to-string
  "(nil true false hello-world)"
  "(nil true false hello-world)")

(clj-parse-deftest clj-parse-to-string-qualified-symbol
  clj-parse-to-string
  "clojure.string/join"
  "clojure.string/join")

(clj-parse-deftest clj-parse-to-string-nested-lists
  clj-parse-to-string
  "((.9 abc (true) (hello)))"
  "((.9 abc (true) (hello)))")

(clj-parse-deftest clj-parse-to-string-strings-1
  clj-parse-to-string
  "\"abc hello \\t\\\"x\""
  "\"abc hello \\t\\\"x\"")

(clj-parse-deftest clj-parse-to-string-strings-2
  clj-parse-to-string
  "(\"---\\f---\\\"-'\\'-\\\\-\\r\\n\")"
  "(\"---\\f---\\\"-'\\'-\\\\-\\r\\n\")")

(clj-parse-deftest clj-parse-to-string-chars-1
  clj-parse-to-string
  "(\\newline \\return \\space \\tab \\a \\b \\c \\u0078 \\o171)"
  "(\\newline \\return \\space \\tab \\a \\b \\c \\u0078 \\o171)")

(clj-parse-deftest clj-parse-to-string-chars-2
  clj-parse-to-string
  "\"\\u0078 \\o171\""
  "\"\\u0078 \\o171\"")

(clj-parse-deftest clj-parse-to-string-keywords
  clj-parse-to-string
  ":foo-bar"
  ":foo-bar")

(clj-parse-deftest clj-parse-to-string-vector
  clj-parse-to-string
  "[123]"
  "[123]")

(clj-parse-deftest clj-parse-to-string-map
  clj-parse-to-string
  "{:count 123}"
  "{:count 123}")

(clj-parse-deftest clj-parse-to-string-set
  clj-parse-to-string
  "#{:x}"
  "#{:x}")

(clj-parse-deftest clj-parse-to-string-discarded
  clj-parse-to-string
  "(10 #_11 12 #_#_ 13 14)"
  "(10 12 14)")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AST

(clj-parse-deftest clj-parse-ast-simple-list
  clj-parse
  "(1 2 3)"
  '((type . :root)
    (subnodes . (((type . :list)
                  (subnodes . (((type . :number) (form . "1") (pos . 2))
                               ((type . :number) (form . "2") (pos . 4))
                               ((type . :number) (form . "3") (pos . 6)))))))))

(provide 'clj-parse-test)

;;; clj-parse-test.el ends here
