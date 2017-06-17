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

(require 'ert)

;; Tried
;; - default-directory
;; - (file-name-directory load-file-name)
;; Neither works so shelling out to `pwd` it is.
(let ((pwd (replace-regexp-in-string "\n\\'" "" (shell-command-to-string "pwd"))))
  (load (concat pwd "/clj-parse.el")))

(ert-run-tests-batch-and-exit)
