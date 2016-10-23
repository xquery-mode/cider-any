(require 'cask)

(let ((source-directory (locate-dominating-file load-file-name "Cask")))
  (cask-initialize source-directory)
  (add-to-list 'load-path source-directory))

(require 'eval-any)
(require 'eval-any-xquery)

(add-hook 'clojure-mode-hook 'cider-mode)

(add-hook 'xquery-mode-hook 'eval-any-mode)

(setq eval-any-xquery-connection
      (list
       :host "localhost"
       :port "8889"
       :user "proofit404"
       :password (with-current-buffer
		     (find-file-noselect (expand-file-name "passwd" (file-name-directory (or load-file-name default-directory))))
		   (buffer-string))
       :content-base "TutorialDB"))
