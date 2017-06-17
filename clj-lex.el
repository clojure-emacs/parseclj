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

(defun clj-lex-whitespace ()
  (let* ((pos (point)))
    (while (or (equal (char-after (point)) ?\ )
               (equal (char-after (point)) ?\t)
               (equal (char-after (point)) ?\n)
               (equal (char-after (point)) ?\r)
               (equal (char-after (point)) ?,))
      (right-char))
    (clj-lex-token :whitespace
                   (buffer-substring-no-properties pos (point))
                   pos)))


(defun clj-lex-number ()
  (let* ((pos (point)))
    (while (and (char-after (point))
                (or (and (<= ?0 (char-after (point))) (<= (char-after (point)) ?9))
                    (eq (char-after (point)) ?.)
                    (eq (char-after (point)) ?M)
                    (eq (char-after (point)) ?r)))
      (right-char))
    (let* ((num-str (buffer-substring-no-properties pos (point))))
      ;; TODO handle radix, bignuM
      (clj-lex-token :number num-str pos))))

(defun clj-lex-next ()
  (if (eq (point) (point-max))
      (clj-lex-token :eof nil (point))
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
        (clj-lex-token :lparen "(" pos))

       ((equal char ?\))
        (right-char)
        (clj-lex-token :rparen ")" pos))

       ((and (<= ?0 char) (<= char ?9))
        (clj-lex-number))

       ":("))))

(provide 'clj-lex)

;;; clj-lex.el ends here
