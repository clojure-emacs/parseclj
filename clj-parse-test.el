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

(ert-deftest clj-parse-test ()
  (with-temp-buffer
    (insert "(1 2 3)")
    (goto-char 1)
    (should (equal (clj-parse) '((1 2 3)))))

  (with-temp-buffer
    (insert "()")
    (goto-char 1)
    (should (equal (clj-parse) '(()))))

  (with-temp-buffer
    (insert "(1)")
    (goto-char 1)
    (should (equal (clj-parse) '((1))))))

;; (ert-deftest clj-parse-test--reduce-list ()
;;   (clj-parse-test--reduce-list ))

(provide 'clj-parse-test)

;;; clj-parse-test.el ends here
