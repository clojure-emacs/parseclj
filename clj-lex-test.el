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
    (should (equal (clj-lex-next) '((type . :whitespace) (form . " \t  \n") (pos . 1))))))


(ert-deftest clj-lex-test-token ()
  (should (equal (clj-lex-token :whitespace ",,," 10)
                 '((type . :whitespace)
                   (form . ",,,")
                   (pos . 10)))))

(provide 'clj-lex-test)

;;; clj-lex-test.el ends here
