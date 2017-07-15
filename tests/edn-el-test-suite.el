;;; edn-el-test-suite.el --- Tests from edn.el

;; Author: Lars Andersen <expez@expez.com>, Arne Brasseur <arne@arnebrasseur.net>

;; Copyright (C) 2015  Lars Andersen

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 'ert)
(require 'edn)

(ert-deftest whitespace ()
  (should (null (clj-edn-read-str "")))
  (should (null (clj-edn-read-str " ")))
  (should (null (clj-edn-read-str "   ")))
  (should (null (clj-edn-read-str "	")))
  (should (null (clj-edn-read-str "		")))
  (should (null (clj-edn-read-str ",")))
  (should (null (clj-edn-read-str ",,,,")))
  (should (null (clj-edn-read-str "	  , ,
")))
  (should (null (clj-edn-read-str"
  ,, 	")))
  (should (equal [a b c d] (clj-edn-read-str "[a ,,,,,, b,,,,,c ,d]"))))

(ert-deftest symbols ()
  :tags '(edn symbol)
  (should (equal 'foo (clj-edn-read-str "foo")))
  (should (equal 'foo\. (clj-edn-read-str "foo.")))
  (should (equal '%foo\. (clj-edn-read-str "%foo.")))
  (should (equal 'foo/bar (clj-edn-read-str "foo/bar")))
  (equal 'some\#sort\#of\#symbol (clj-edn-read-str "some#sort#of#symbol"))
  (equal 'truefalse (clj-edn-read-str "truefalse"))
  (equal 'true. (clj-edn-read-str "true."))
  (equal '/ (clj-edn-read-str "/"))
  (should (equal '.true (clj-edn-read-str ".true")))
  (should (equal 'some:sort:of:symbol (clj-edn-read-str "some:sort:of:symbol")))
  (equal 'foo-bar (clj-edn-read-str "foo-bar"))
  (should (equal '+some-symbol (clj-edn-read-str "+some-symbol")))
  (should (equal '-symbol (clj-edn-read-str "-symbol"))))

(ert-deftest booleans ()
  :tags '(edn boolean)
  (should (equal t (clj-edn-read-str "true")))
  (should (equal nil (clj-edn-read-str "false "))))

(ert-deftest characters ()
  :tags '(edn characters)
  (should (equal 97 (clj-edn-read-str "\\a")))
  (should (equal 960 (clj-edn-read-str "\\u03C0")))
  ;;(should (equal 'newline (clj-edn-read-str "\\newline")))
  )

(ert-deftest elision ()
  :tags '(edn elision)
  (should-not (clj-edn-read-str "#_foo"))
  (should-not (clj-edn-read-str "#_ 123"))
  (should-not (clj-edn-read-str "#_:foo"))
  (should-not (clj-edn-read-str "#_ \\a"))
  (should-not (clj-edn-read-str "#_
\"foo\""))
  (should-not (clj-edn-read-str "#_ (1 2 3)"))
  (should (equal '(1 3) (clj-edn-read-str "(1 #_ 2 3)")))
  (should (equal '[1 2 3 4] (clj-edn-read-str "[1 2 #_[4 5 6] 3 4]")))
  (should (map-equal (make-seeded-hash-table :foo :bar)
                     (clj-edn-read-str "{:foo #_elided :bar}")))
  (should (equal (edn-list-to-set '(1 2 3 4))
                 (clj-edn-read-str "#{1 2 #_[1 2 3] 3 #_ (1 2) 4}")))
  (should (equal [a d] (clj-edn-read-str "[a #_ ;we are discarding what comes next
 c d]"))))

(ert-deftest string ()
  :tags '(edn string)
  (should (equal "this is a string" (clj-edn-read-str "\"this is a string\"")))
  (should (equal "this has an escaped \"quote in it"
                 (clj-edn-read-str "\"this has an escaped \\\"quote in it\"")))
  (should (equal "foo\tbar" (clj-edn-read-str "\"foo\\tbar\"")))
  (should (equal "foo\nbar" (clj-edn-read-str "\"foo\\nbar\"")))
  (should (equal "this is a string \\ that has an escaped backslash"
                 (clj-edn-read-str "\"this is a string \\\\ that has an escaped backslash\"")))
  (should (equal "[" (clj-edn-read-str "\"[\""))))

(ert-deftest keywords ()
  :tags '(edn keywords)
  (should (equal :namespace\.of\.some\.length/keyword-name
                 (clj-edn-read-str ":namespace.of.some.length/keyword-name")))
  (should (equal :\#/\# (clj-edn-read-str ":#/#")))
  (should (equal :\#/:a (clj-edn-read-str ":#/:a")))
  (should (equal :\#foo (clj-edn-read-str ":#foo"))))

(ert-deftest integers ()
  :tags '(edn integers)
  (should (= 0 (clj-edn-read-str "0")))
  (should (= 0 (clj-edn-read-str "+0")))
  (should (= 0 (clj-edn-read-str "-0")))
  (should (= 100 (clj-edn-read-str "100")))
  (should (= -100 (clj-edn-read-str "-100"))))

(ert-deftest floats ()
  :tags '(edn floats)
  (should (= 12.32 (clj-edn-read-str "12.32")))
  (should (= -12.32 (clj-edn-read-str "-12.32")))
  (should (= 9923.23 (clj-edn-read-str "+9923.23")))
  (should (= 4.5e+044 (clj-edn-read-str "45e+43")))
  (should (= -4.5e-042 (clj-edn-read-str "-45e-43")))
  (should (= 4.5e+044 (clj-edn-read-str "45E+43"))))

(ert-deftest lists ()
  :tags '(edn lists)
  (should-not (clj-edn-read-str "()"))
  (should (equal '(1 2 3) (clj-edn-read-str "( 1 2 3)")))
  (should (equal '(12.1 ?a foo :bar) (clj-edn-read-str "(12.1 \\a foo :bar)")))
  (should (equal '((:foo bar :bar 12)) (clj-edn-read-str "( (:foo bar :bar 12))")))
  (should (equal
           '(defproject com\.thortech/data\.edn "0.1.0-SNAPSHOT")
           (clj-edn-read-str "(defproject com.thortech/data.edn \"0.1.0-SNAPSHOT\")"))))

(ert-deftest vectors ()
  :tags '(edn vectors)
  (should (equal [] (clj-edn-read-str "[]")))
  (should (equal [] (clj-edn-read-str "[ ]")))
  (should (equal '[1 2 3] (clj-edn-read-str "[ 1 2 3 ]")))
  (should (equal '[12.1 ?a foo :bar] (clj-edn-read-str "[ 12.1 \\a foo :bar]")))
  (should (equal '[[:foo bar :bar 12]] (clj-edn-read-str "[[:foo bar :bar 12]]")))
  (should (equal '[( :foo bar :bar 12 ) "foo"]
                 (clj-edn-read-str "[(:foo bar :bar 12) \"foo\"]")))
  (should (equal '[/ \. * ! _ \? $ % & = - +]
                 (clj-edn-read-str "[/ . * ! _ ? $ % & = - +]")))
  (should (equal
           ;;[99 newline return space tab]
           [99 10 13 32 9]
           (clj-edn-read-str "[\\c \\newline \\return \\space \\tab]"))))

(defun map-equal (m1 m2)
  (and (and (hash-table-p m1) (hash-table-p m2))
       (eq (hash-table-test m1) (hash-table-test m2))
       (= (hash-table-count m1) (hash-table-count m2))
       (equal (hash-table-keys m1) (hash-table-keys m2))
       (equal (hash-table-values m1) (hash-table-values m2))))

(defun make-seeded-hash-table (&rest keys-and-values)
  (let ((m (make-hash-table :test #'equal)))
    (while keys-and-values
      (puthash (pop keys-and-values) (pop keys-and-values) m))
    m))

(ert-deftest maps ()
  :tags '(edn maps)
  (should (hash-table-p (clj-edn-read-str "{ }")))
  (should (hash-table-p (clj-edn-read-str "{}")))
  (should (map-equal (make-seeded-hash-table :foo :bar :baz :qux)
                     (clj-edn-read-str "{ :foo :bar :baz :qux}")))
  (should (map-equal (make-seeded-hash-table 1 "123" 'vector [1 2 3])
                     (clj-edn-read-str "{ 1 \"123\" vector [1 2 3]}")))
  (should (map-equal (make-seeded-hash-table [1 2 3] "some numbers")
                     (clj-edn-read-str "{[1 2 3] \"some numbers\"}"))))

(ert-deftest sets ()
  :tags '(edn sets)
  (should (edn-set-p (clj-edn-read-str "#{}")))
  (should (edn-set-p (clj-edn-read-str "#{ }")))
  (should (equal (edn-list-to-set '(1 2 3)) (clj-edn-read-str "#{1 2 3}")))
  (should (equal (edn-list-to-set '(1 [1 2 3] 3)) (clj-edn-read-str "#{1 [1 2 3] 3}"))))

(ert-deftest comment ()
  :tags '(edn comments)
  (should-not (clj-edn-read-str ";nada"))
  (should (equal 1 (clj-edn-read-str ";; comment
1")))
  (should (equal [1 2 3] (clj-edn-read-str "[1 2 ;comment to eol
3]")))
  (should (equal '[valid more items] (clj-edn-read-str "[valid;touching trailing comment
 more items]")))
  (should (equal [valid vector more vector items] (clj-edn-read-str "[valid vector
 ;;comment in vector
 more vector items]"))))

(defun test-val-passed-to-handler (val)
  (should (listp val))
  (should (= (length val) 2))
  (should (= 1 (car val)))
  1)

(setq clj-edn-test-extra-handlers
      (a-list
       'my/type #'test-val-passed-to-handler
       'my/other-type (lambda (val) 2)))

(ert-deftest tags ()
  :tags '(edn tags)
  (should-error (clj-edn-read-str "#my/type value" clj-edn-test-extra-handlers))
  (should (= 1 (clj-edn-read-str "#my/type (1 2)" clj-edn-test-extra-handlers)))
  (should (= 2 (clj-edn-read-str "#my/other-type {:foo :bar}" clj-edn-test-extra-handlers))))

(ert-deftest roundtrip ()
  :tags '(edn roundtrip)
  (let ((data [1 2 3 :foo (4 5) qux "quux"]))
    (should (equal data (clj-edn-read-str (edn-print-string data))))
    (should (map-equal (make-seeded-hash-table :foo :bar)
                       (clj-edn-read-str (edn-print-string (make-seeded-hash-table :foo :bar)))))
    (should (equal (edn-list-to-set '(1 2 3 [3 1.11]))
                   (clj-edn-read-str (edn-print-string (edn-list-to-set '(1 2 3 [3 1.11]))))))
    (should-error (clj-edn-read-str "#myapp/Person {:first \"Fred\" :last \"Mertz\"}"))))

(ert-deftest inst ()
  :tags '(edn inst)
  (let* ((inst-str "#inst \"1985-04-12T23:20:50.52Z\"")
         (inst (clj-edn-read-str inst-str))
         (time (date-to-time "1985-04-12T23:20:50.52Z")))
    (should (edn-inst-p inst))
    (should (equal time (edn-inst-to-time inst)))))

(ert-deftest uuid ()
  :tags '(edn uuid)
  (let* ((str "f81d4fae-7dec-11d0-a765-00a0c91e6bf6")
         (uuid (clj-edn-read-str (concat "#uuid \"" str "\""))))
    (should (edn-uuid-p uuid))))

;; (ert-deftest invalid-edn ()
;;   (should-error (clj-edn-read-str "///"))
;;   (should-error (clj-edn-read-str "~cat"))
;;   (should-error (clj-edn-read-str "foo/bar/baz/qux/quux"))
;;   (should-error (clj-edn-read-str "#foo/"))
;;   (should-error (clj-edn-read-str "foo/"))
;;   (should-error (clj-edn-read-str ":foo/"))
;;   (should-error (clj-edn-read-str "#/foo"))
;;   (should-error (clj-edn-read-str "/symbol"))
;;   (should-error (clj-edn-read-str ":/foo"))
;;   (should-error (clj-edn-read-str "+5symbol"))
;;   (should-error (clj-edn-read-str ".\\newline"))
;;   (should-error (clj-edn-read-str "0cat"))
;;   (should-error (clj-edn-read-str "-4cats"))
;;   (should-error (clj-edn-read-str ".9"))
;;   (should-error (clj-edn-read-str ":keyword/with/too/many/slashes"))
;;   (should-error (clj-edn-read-str ":a.b.c/"))
;;   (should-error (clj-edn-read-str "\\itstoolong"))
;;   (should-error (clj-edn-read-str ":#/:"))
;;   (should-error (clj-edn-read-str "/foo//"))
;;   (should-error (clj-edn-read-str "///foo"))
;;   (should-error (clj-edn-read-str ":{}"))
;;   (should-error (clj-edn-read-str "//"))
;;   (should-error (clj-edn-read-str "##"))
;;   (should-error (clj-edn-read-str "::"))
;;   (should-error (clj-edn-read-str "::a"))
;;   (should-error (clj-edn-read-str ".5symbol"))
;;   (should-error (clj-edn-read-str "{ \"foo\""))
;;   (should-error (clj-edn-read-str "{ \"foo\" :bar"))
;;   (should-error (clj-edn-read-str "{"))
;;   (should-error (clj-edn-read-str ":{"))
;;   (should-error (clj-edn-read-str "{{"))
;;   (should-error (clj-edn-read-str "}"))
;;   (should-error (clj-edn-read-str ":}"))
;;   (should-error (clj-edn-read-str "}}"))
;;   (should-error (clj-edn-read-str "#:foo"))
;;   (should-error (clj-edn-read-str "\\newline."))
;;   (should-error (clj-edn-read-str "\\newline0.1"))
;;   (should-error (clj-edn-read-str "^"))
;;   (should-error (clj-edn-read-str ":^"))
;;   (should-error (clj-edn-read-str "_:^"))
;;   (should-error (clj-edn-read-str "#{{[}}"))
;;   (should-error (clj-edn-read-str "[}"))
;;   (should-error (clj-edn-read-str "@cat")))

;;; edn-el-test-suite.el ends here
