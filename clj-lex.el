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
    (while (and (char-after (point))
                (or (and (<= ?0 (char-after (point))) (<= (char-after (point)) ?9))
                    (eq (char-after (point)) ?.)
                    (eq (char-after (point)) ?M)
                    (eq (char-after (point)) ?r)))
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

       ((and (<= ?0 char) (<= char ?9))
        (clj-lex-number))

       ":("))))

(provide 'clj-lex)

;;; clj-lex.el ends here
