;;; clj-parse.el --- Clojure/EDN parser

;; Copyright (C) 2017  Arne Brasseur

;; Author: Arne Brasseur <arne@arnebrasseur.net>
;; Keywords: lisp
;; Package-Requires: ((dash "") (let-alist ""))

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

(defun clj-parse ()
  (clj-parse* 'clj-parse-elisp-reducer))

(defun clj-parse-elisp-reducer (type value)
  (case type
    (:whitespace :ws)
    (:number value)
    (:list value)))

(defun clj-parse* (reducer)
  (let ((stack nil)
        (token (clj-lex-next)))
    (while (not (eq (alist-get 'type token) :eof))
      ;;(prin1 (alist-get 'type token))
      (print token)
      ;; (print stack)
      (let-alist token
        (case .type
          (:whitespace
           (push (funcall reducer :whitespace .form) stack))
          (:number
           (push (funcall reducer :number .value) stack))
          (:lparen
           (push token stack))
          (:rparen
           (let ((list nil))
             (while (not (and (listp (car stack)) (eq (alist-get 'type (car stack)) :lparen)))
               (push (pop stack) list))
             (pop stack) ;; :lparen
             (print list)
             (push (funcall reducer :list list) stack)))))
      (setq token (clj-lex-next)))
    stack))

(defun clj-lex-whitespace ()
  (let* ((pos (point)))
    (while (or (equal (char-after (point)) ?\ )
               (equal (char-after (point)) ?\t)
               (equal (char-after (point)) ?\n)
               (equal (char-after (point)) ?\r)
               (equal (char-after (point)) ?,))
      (right-char))
    `((type . :whitespace) (form . ,(buffer-substring-no-properties pos (point))) (pos . ,pos))))


(defun clj-lex-number ()
  (let* ((pos (point)))
    (while (or (<= ?0 (char-after (point)) ?9)
               (eq (char-after (point)) ?.)
               (eq (char-after (point)) ?M)
               (eq (char-after (point)) ?r))
      (right-char))
    (let* ((num-str (buffer-substring-no-properties pos (point))))
      ;; TODO handle radix, bignuM
      `((type . :number)
        (value . ,(string-to-number num-str))
        (form . ,num-str)
        (pos . ,pos)))))

(defun clj-lex-next ()
  (if (eq (point) (point-max))
      `((type . :eof) (pos . ,(point)))
    (let ((char (char-after (point)))
          (pos  (point)))
      (cond
       ((or (equal char ?\ )
            (equal char ?\t)
            (equal char ?\n)
            (equal char ?\r)
            (equal char ?,))
        (clj-lex-whitespace))

       ((equal char ?\()
        (right-char)
        `((type . :lparen) (pos . ,pos)))

       ((equal char ?\))
        (right-char)
        `((type . :rparen) (pos . ,pos)))

       ((<= ?0 char ?9)
        (clj-lex-number))

       ":("))))

(ert-deftest clj-parse-test ()
  (with-temp-buffer
    (insert "()")
    (goto-char 1)
    (should (equal (clj-parse) '())))

  (with-temp-buffer
    (insert "(1)")
    (goto-char 1)
    (should (equal (clj-parse) '(1)))))

(ert-deftest clj-lex-next-test ()
  (with-temp-buffer
    (insert "()")
    (goto-char 1)
    (should (equal (clj-lex-next) '((type . :lparen) (pos . 1))))
    (should (equal (clj-lex-next) '((type . :rparen) (pos . 2))))
    (should (equal (clj-lex-next) '((type . :eof) (pos . 3)))))

  (with-temp-buffer
    (insert "123")
    (goto-char 1)
    (should (equal (clj-lex-next) '((type . :number)
                                       (value . 123)
                                       (form . "123")
                                       (pos . 1)))))

  (with-temp-buffer
    (insert " \t  \n")
    (goto-char 1)
    (should (equal (clj-lex-next) '((type . :whitespace) (form . " \t  \n") (pos . 1))))))



(provide 'clj-parse)
;;; clj-parse.el ends here
