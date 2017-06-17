;;; clj-lex.el --- Clojure/EDN parser

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

(require 'dash)

(defun clj-lex-token (type form pos &rest args)
  `((type . ,type)
    (form . ,form)
    (pos . , pos)
    ,@(mapcar (lambda (pair)
                (cons (car pair) (cadr pair)))
              (-partition 2 args))))

(defun clj-lex-at-whitespace? ()
  (let ((char (char-after (point))))
    (or (equal char ?\ )
        (equal char ?\t)
        (equal char ?\n)
        (equal char ?\r)
        (equal char ?,))))

(defun clj-lex-at-eof? ()
  (eq (point) (point-max)))

(defun clj-lex-whitespace ()
  (let ((pos (point)))
    (while (clj-lex-at-whitespace?)
      (right-char))
    (clj-lex-token :whitespace
                   (buffer-substring-no-properties pos (point))
                   pos)))

(defun clj-lex-number ()
  (let ((pos (point)))
    (while (and (char-after (point))
                (or (and (<= ?0 (char-after (point))) (<= (char-after (point)) ?9))
                    (eq (char-after (point)) ?.)
                    (eq (char-after (point)) ?M)
                    (eq (char-after (point)) ?r)))
      (right-char))
    (clj-lex-token :number
                   (buffer-substring-no-properties pos (point))
                   pos)))


(defun clj-lex-digit? (char)
  (and char (<= ?0 char) (<= char ?9)))

(defun clj-lex-at-number? ()
  (let ((char (char-after (point))))
    (or (clj-lex-digit? char)
        (and (member char '(?- ?+ ?.))
             (clj-lex-digit? (char-after (1+ (point))))))))

(defun clj-lex-symbol-start? (char)
  "Symbols begin with a non-numeric character and can contain
   alphanumeric characters and . * + ! - _ ? $ % & = < >. If -, +
   or . are the first character, the second character (if any)
   must be non-numeric."
  (not (not (and char
                 (or (and (<= ?a char) (<= char ?z))
                     (and (<= ?A char) (<= char ?Z))
                     (member char '(?. ?* ?+ ?! ?- ?_ ?? ?$ ?% ?& ?= ?< ?> ?/)))))))

(defun clj-lex-symbol-rest? (char)
  (or (clj-lex-symbol-start? char)
      (clj-lex-digit? char)))

(defun clj-lex-symbol ()
  (let ((pos (point)))
    (right-char)
    (while (clj-lex-symbol-rest? (char-after (point)))
      (right-char))
    (let ((sym (buffer-substring-no-properties pos (point))))
      (cond
       ((equal sym "nil") (clj-lex-token :nil "nil" pos))
       ((equal sym "true") (clj-lex-token :true "true" pos))
       ((equal sym "false") (clj-lex-token :false "false" pos))
       (t (clj-lex-token :symbol sym pos))))))

(defun clj-lex-string ()
  (let ((pos (point)))
    (right-char)
    (while (not (or (equal (char-after (point)) ?\") (clj-lex-at-eof?)))
      (message (buffer-substring-no-properties pos (point)))
      (if (equal (char-after (point)) ?\\)
          (right-char 2)
        (right-char)))
    (if (equal (char-after (point)) ?\")
        (progn
          (right-char)
          (clj-lex-token :string (buffer-substring-no-properties pos (point)) pos))
      (clj-lex-token :lex-error (buffer-substring-no-properties pos (point)) pos))))

(defun clj-lex-lookahead (n)
  (buffer-substring-no-properties (point) (min (+ (point) n) (point-max))))

(defun clj-lex-character ()
  (let ((pos (point)))
    (right-char)
    (cond
     ((equal (clj-lex-lookahead 3) "tab")
      (right-char 3)
      (clj-lex-token :character (buffer-substring-no-properties pos (point)) pos))

     ((equal (clj-lex-lookahead 5) "space")
      (right-char 5)
      (clj-lex-token :character (buffer-substring-no-properties pos (point)) pos))

     ((equal (clj-lex-lookahead 6) "return")
      (right-char 6)
      (clj-lex-token :character (buffer-substring-no-properties pos (point)) pos))

     ((equal (clj-lex-lookahead 7) "newline")
      (right-char 7)
      (clj-lex-token :character (buffer-substring-no-properties pos (point)) pos))

     ((equal (char-after (point)) ?u)
      (right-char 5)
      (clj-lex-token :character (buffer-substring-no-properties pos (point)) pos))

     ((equal (char-after (point)) ?o)
      (right-char 4)
      (clj-lex-token :character (buffer-substring-no-properties pos (point)) pos))

     (t
      (right-char)
      (clj-lex-token :character (buffer-substring-no-properties pos (point)) pos)))))

(defun clj-lex-keyword ()
  (let ((pos (point)))
    (right-char)
    (when (equal (char-after (point)) ?:)
      (right-char))
    (if (clj-lex-symbol-start? (char-after (point)))
        (progn
          (while (clj-lex-symbol-rest? (char-after (point)))
            (right-char))
          (clj-lex-token :keyword (buffer-substring-no-properties pos (point)) pos))
      (progn
        (right-char)
        (clj-lex-token :lex-error (buffer-substring-no-properties pos (point)) pos 'error-type :invalid-keyword)))))

(defun clj-lex-next ()
  (if (clj-lex-at-eof?)
      (clj-lex-token :eof nil (point))
    (let ((char (char-after (point)))
          (pos  (point)))
      (cond
       ((clj-lex-at-whitespace?)
        (clj-lex-whitespace))

       ((equal char ?\()
        (right-char)
        (clj-lex-token :lparen "(" pos))

       ((equal char ?\))
        (right-char)
        (clj-lex-token :rparen ")" pos))

       ((equal char ?\[)
        (right-char)
        (clj-lex-token :lbracket "[" pos))

       ((equal char ?\])
        (right-char)
        (clj-lex-token :rbracket "]" pos))

       ((equal char ?{)
        (right-char)
        (clj-lex-token :lbrace "{" pos))

       ((equal char ?})
        (right-char)
        (clj-lex-token :rbrace "}" pos))

       ((clj-lex-at-number?)
        (clj-lex-number))

       ((clj-lex-symbol-start? char)
        (clj-lex-symbol))

       ((equal char ?\")
        (clj-lex-string))

       ((equal char ?\\)
        (clj-lex-character))

       ((equal char ?:)
        (clj-lex-keyword))

       ((equal char ?#)
        (right-char)
        (let ((char (char-after (point))))
          (cl-case char
            (?{
             (right-char)
             (clj-lex-token :set "#{" pos)))))

       ":("))))

(provide 'clj-lex)

;;; clj-lex.el ends here
