;;; init.el --- minimal cider-any configuration

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'cask)

(let ((source-directory (locate-dominating-file load-file-name "Cask")))
  (cask-initialize source-directory)
  (add-to-list 'load-path source-directory))

(require 'cider-any)

(add-hook 'clojure-mode-hook 'cider-mode)

(add-hook 'xquery-mode-hook 'cider-any-mode)

(defun cider-any-xquery (command &rest args)
  "Eval XQuery in Cider.
Read backend reference for COMMAND and ARGS purpose description."
  (cl-case command
    (check (eq major-mode 'xquery-mode))
    (init "(defn foo [x] (println x))")
    (eval "(default.core/foo \"%s\")")))

(add-to-list 'cider-any-backends 'cider-any-xquery)

(provide 'init)

;;; init.el ends here
