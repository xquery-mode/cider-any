;;; init.el --- minimal cider-any configuration

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'cask)

(let ((source-directory (locate-dominating-file load-file-name "Cask")))
  (cask-initialize source-directory)
  (add-to-list 'load-path source-directory))

(require 'cider-any)

(add-hook 'xquery-mode-hook 'cider-any-mode)

(defun cider-any-xquery (command &rest args)
  (cl-case command
    (check (eq major-mode 'xquery-mode))
    (eval (message "It works"))))

(add-to-list 'cider-any-backends 'cider-any-xquery)

(provide 'init)

;;; init.el ends here
