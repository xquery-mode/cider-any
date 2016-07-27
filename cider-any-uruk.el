;;; init.el --- Evaluate XQuery with uruk in the cider  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'cider-any)

(defun cider-any-uruk (command &rest args)
  "Eval XQuery in Cider."
  (cl-case command
    (check (eq major-mode 'xquery-mode))
    (init "(defn foo [x] (println x) x)")
    (eval "(foo \"%s\")")
    (handle (message ">>> %s <<<" args))
    (handle-init (message ">>> %s <<<" args))))

(add-to-list 'cider-any-backends 'cider-any-uruk)

(provide 'cider-any-uruk)

;;; cider-any-uruk.el ends here
