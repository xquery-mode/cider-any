;;; init.el --- minimal cider-eval-any configuration

;;; Commentary:

;;; Code:

(require 'cask)

(let ((source-directory (locate-dominating-file load-file-name "Cask")))
  (cask-initialize source-directory)
  (add-to-list 'load-path source-directory))

(require 'cider-eval-any)

(provide 'init)

;;; init.el ends here
