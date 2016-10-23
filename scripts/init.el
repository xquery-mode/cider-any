(require 'cask)

(let* ((source-directory (locate-dominating-file load-file-name "Cask"))
       (examples-directory (expand-file-name "examples" source-directory)))
  (cask-initialize source-directory)
  (add-to-list 'load-path source-directory)
  (add-to-list 'load-path examples-directory))

(require 'eval-any)
(require 'eval-any-xquery)
(require 'eval-any-xquery-pprint)
(require 'eval-any-xquery-to-file)

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
