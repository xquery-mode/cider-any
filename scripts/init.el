;;; init.el --- minimal cider-eval-any configuration

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'cask)

(let ((source-directory (locate-dominating-file load-file-name "Cask")))
  (cask-initialize source-directory)
  (add-to-list 'load-path source-directory))

(require 'cider-eval-any)

(add-hook 'xquery-mode-hook 'cider-eval-any-mode)

(defun cider-eval-any-xquery (command &rest args)
  (cl-case command
    (check (eq major-mode 'xquery-mode))
    (eval (message "It works"))))

(add-to-list 'cider-eval-any-backends 'cider-eval-any-xquery)

(provide 'init)

;;; init.el ends here
