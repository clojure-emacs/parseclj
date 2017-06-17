;; Script used for the Travis build

(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ;;("melpa" . "https://melpa.org/packages/")
        ;;("melpa-stable" . "https://stable.melpa.org/packages/")
        ))

(package-initialize)
(package-refresh-contents)

;; Emacs before 25.1
(when (not (fboundp 'let-alist))
  (package-install 'let-alist))

(package-install 'package-lint)

(require 'ert)

;; Tried
;; - default-directory
;; - (file-name-directory load-file-name)
;; Neither works so shelling out to `pwd` it is.
(let ((pwd (replace-regexp-in-string "\n\\'" "" (shell-command-to-string "pwd"))))
  (load (concat pwd "/clj-parse.el")))

(if (getenv "CLJ_PARSE_LINT")
    (let ((success t))
      (dolist (file '("clj-parse.el"))
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
  (ert-run-tests-batch-and-exit))
