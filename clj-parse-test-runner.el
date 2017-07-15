;; Script used for the Travis build

(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("plexus-elpa" . "https://plexus.github.io/elpa/")))


(package-initialize)
(package-refresh-contents)

;; Emacs before 25.1
(when (not (fboundp 'let-alist))
  (package-install 'let-alist))

(package-install 'package-lint)
(package-install 'a)
(package-install 'dash)
(package-install 'edn) ;; required for the edn test suite

(setq clj-parse-load-files '("clj-parse.el"
                             "clj-lex.el"
                             "tests/clj-parse-test.el"
                             "tests/clj-lex-test.el"
                             "tests/edn-el-test-suite.el"))

(let ((pwd (replace-regexp-in-string "\n\\'" "" (shell-command-to-string "pwd"))))

  (add-to-list 'load-path pwd)

  (dolist (file clj-parse-load-files)
    (load (concat pwd "/" file)))

  (if (getenv "CLJ_PARSE_LINT")
      (let ((success t))
        (dolist (file clj-parse-load-files)
          (with-temp-buffer
            (insert-file-contents file t)
            (emacs-lisp-mode)
            (let ((checking-result (package-lint-buffer)))
              (when checking-result
                (setq success nil)
                (message "In `%s':" file)
                (pcase-dolist (`(,line ,col ,type ,message) checking-result)
                  (message "  at %d:%d: %s: %s" line col type message))))))
        (kill-emacs (if success 0 1)))
    (ert-run-tests-batch-and-exit)))
