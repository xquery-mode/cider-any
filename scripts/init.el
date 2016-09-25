(require 'cask)

(let ((source-directory (locate-dominating-file load-file-name "Cask")))
  (cask-initialize source-directory)
  (add-to-list 'load-path source-directory))

(require 'cider-any)
(require 'cider-any-uruk)

(add-hook 'clojure-mode-hook 'cider-mode)

(add-hook 'xquery-mode-hook 'cider-any-mode)

(setq cider-any-uruk-uri "xdbc://localhost:8889/"
      cider-any-uruk-user "proofit404"
      cider-any-uruk-password (with-current-buffer
				  (find-file-noselect
                                   (expand-file-name "passwd" (file-name-directory (or load-file-name default-directory))))
				(buffer-string))
      cider-any-uruk-content-base "TutorialDB")
