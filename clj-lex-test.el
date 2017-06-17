;;; clj-lex-test.el --- Clojure/EDN parser

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

(require 'clj-lex)
(require 'ert)

(ert-deftest clj-lex-test-next ()
  (with-temp-buffer
    (insert "()")
    (goto-char 1)
    (should (equal (clj-lex-next) '((type . :lparen) (form . "(") (pos . 1))))
    (should (equal (clj-lex-next) '((type . :rparen) (form . ")") (pos . 2))))
    (should (equal (clj-lex-next) '((type . :eof) (form . nil) (pos . 3)))))

  (with-temp-buffer
    (insert "123")
    (goto-char 1)
    (should (equal (clj-lex-next) '((type . :number)
                                    (form . "123")
                                    (pos . 1)))))

  (with-temp-buffer
    (insert " \t  \n")
    (goto-char 1)
    (should (equal (clj-lex-next) '((type . :whitespace) (form . " \t  \n") (pos . 1)))))

  (with-temp-buffer
    (insert "nil")
    (goto-char 1)
    (should (equal (clj-lex-next) '((type . :nil) (form . "nil") (pos . 1)))))

  (with-temp-buffer
    (insert "true")
    (goto-char 1)
    (should (equal (clj-lex-next) '((type . :true) (form . "true") (pos . 1)))))

  (with-temp-buffer
    (insert "false")
    (goto-char 1)
    (should (equal (clj-lex-next) '((type . :false) (form . "false") (pos . 1)))))

  (with-temp-buffer
    (insert "hello-world")
    (goto-char 1)
    (should (equal (clj-lex-next) '((type . :symbol) (form . "hello-world") (pos . 1)))))

  (with-temp-buffer
    (insert "\\newline\\return\\space\\tab\\a\\b\\c")
    (goto-char 1)
    (should (equal (clj-lex-next) (clj-lex-token :character "\\newline" 1)))
    (should (equal (clj-lex-next) (clj-lex-token :character "\\return" 9)))
    (should (equal (clj-lex-next) (clj-lex-token :character "\\space" 16)))
    (should (equal (clj-lex-next) (clj-lex-token :character "\\tab" 22)))
    (should (equal (clj-lex-next) (clj-lex-token :character "\\a" 26)))
    (should (equal (clj-lex-next) (clj-lex-token :character "\\b" 28)))
    (should (equal (clj-lex-next) (clj-lex-token :character "\\c" 30))))

  (with-temp-buffer
    (insert "\\newline\\return\\space\\tab\\a\\b\\c")
    (goto-char 1)
    (should (equal (clj-lex-next) (clj-lex-token :character "\\newline" 1)))
    (should (equal (clj-lex-next) (clj-lex-token :character "\\return" 9)))
    (should (equal (clj-lex-next) (clj-lex-token :character "\\space" 16)))
    (should (equal (clj-lex-next) (clj-lex-token :character "\\tab" 22)))
    (should (equal (clj-lex-next) (clj-lex-token :character "\\a" 26)))
    (should (equal (clj-lex-next) (clj-lex-token :character "\\b" 28)))
    (should (equal (clj-lex-next) (clj-lex-token :character "\\c" 30))))

  (with-temp-buffer
    (insert "\\u0078\\o170")
    (goto-char 1)
    (should (equal (clj-lex-next) (clj-lex-token :character "\\u0078" 1)))
    (should (equal (clj-lex-next) (clj-lex-token :character "\\o170" 7))))

  (with-temp-buffer
    (insert "\"\\u0078\\o170\"")
    (goto-char 1)
    (should (equal (clj-lex-next) (clj-lex-token :string "\"\\u0078\\o170\"" 1))))

  (with-temp-buffer
    (insert ":hello-world")
    (goto-char 1)
    (should (equal (clj-lex-next) (clj-lex-token :keyword ":hello-world" 1))))

  (with-temp-buffer
    (insert "::hello-world")
    (goto-char 1)
    (should (equal (clj-lex-next) (clj-lex-token :keyword "::hello-world" 1))))

  (with-temp-buffer
    (insert ":::hello-world")
    (goto-char 1)
    (should (equal (clj-lex-next) (clj-lex-token :lex-error ":::" 1 'error-type :invalid-keyword))))

  (with-temp-buffer
    (insert "[123]")
    (goto-char 1)
    (should (equal (clj-lex-next) (clj-lex-token :lbracket "[" 1)))
    (should (equal (clj-lex-next) (clj-lex-token :number "123" 2)))
    (should (equal (clj-lex-next) (clj-lex-token :rbracket "]" 5)))))

(ert-deftest clj-lex-test-at-number? ()
  (dolist (str '("123" ".9" "+1" "0" "-456"))
    (with-temp-buffer
      (insert str)
      (goto-char 1)
      (should (equal (clj-lex-at-number?) t))))

  (dolist (str '("a123" "$.9" "+/1" "++0" "-"))
    (with-temp-buffer
      (insert str)
      (goto-char 1)
      (should (equal (clj-lex-at-number?) nil)))))

(ert-deftest clj-lex-test-token ()
  (should (equal (clj-lex-token :whitespace ",,," 10)
                 '((type . :whitespace)
                   (form . ",,,")
                   (pos . 10)))))

(ert-deftest clj-lex-test-digit? ()
  (should (equal (clj-lex-digit? ?0) t))
  (should (equal (clj-lex-digit? ?5) t))
  (should (equal (clj-lex-digit? ?9) t))
  (should (equal (clj-lex-digit? ?a) nil))
  (should (equal (clj-lex-digit? ?-) nil)))

(ert-deftest clj-lex-test-symbol-start? ()
  (should (equal (clj-lex-symbol-start? ?0) nil))
  (should (equal (clj-lex-symbol-start? ?a) t))
  (should (equal (clj-lex-symbol-start? ?A) t))
  (should (equal (clj-lex-symbol-start? ?.) t))
  (should (equal (clj-lex-symbol-start? ?~) nil))
  (should (equal (clj-lex-symbol-start? ? ) nil)))

(ert-deftest clj-lex-test-symbol-rest? ()
  (should (equal (clj-lex-symbol-rest? ?0) t))
  (should (equal (clj-lex-symbol-rest? ?a) t))
  (should (equal (clj-lex-symbol-rest? ?A) t))
  (should (equal (clj-lex-symbol-rest? ?.) t))
  (should (equal (clj-lex-symbol-rest? ?~) nil))
  (should (equal (clj-lex-symbol-rest? ? ) nil)))

(ert-deftest clj-lex-test-string ()
  (with-temp-buffer
    (insert "\"abc\"")
    (goto-char 1)
    (should (equal (clj-lex-string) (clj-lex-token :string "\"abc\"" 1))))

  (with-temp-buffer
    (insert "\"abc")
    (goto-char 1)
    (should (equal (clj-lex-string) (clj-lex-token :lex-error "\"abc" 1))))

  (with-temp-buffer
    (insert "\"abc\\\"\"")"abc\""
    (goto-char 1)
    (should (equal (clj-lex-string) (clj-lex-token :string "\"abc\\\"\"" 1)))))

(provide 'clj-lex-test)

;;; clj-lex-test.el ends here
