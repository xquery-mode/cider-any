;;; cider-eval-any.el --- Evaluate any buffer in cider.

;; Copyright (C) 2016 by Artem Malyshev

;; Author: Artem Malyshev <proofit404@gmail.com>
;; URL: https://github.com/xquery-mode/cider-eval-any
;; Version: 0.0.1
;; Package-Requires: ((cider "0.13.0"))

;;; Commentary:

;;; Code:

(require 'cider)

(defgroup cider-eval-any nil
  "Evaluate any buffer in cider."
  :group 'cider)

(defcustom cider-eval-any-backends nil
  "The list of active backends.

Only one backend is used at a time.  Each backend is a function
that takes a variable number of arguments.  The first argument is
the command requested from the backend.  It is one of the
following:

`check': The backend should return t in the case it can eval in
current context.  Returning nil from this command passe control
to the next backend.

`eval' Request to perform evaluation of current context.  The
second argument is the context type passed as a symbol.
`buffer', `function', `line' and `region' are default context
types."
  :type '(repeat :tag "User defined" (function)))

(defvar cider-eval-any-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'cider-eval-any-buffer)
    (define-key map (kbd "C-M-x") 'cider-eval-any-function)
    (define-key map (kbd "C-c C-l") 'cider-eval-any-line)
    (define-key map (kbd "C-c C-r") 'cider-eval-any-region)
    map)
  "Keymap for `cider-eval-any-mode'.")

(defun cider-eval-any-buffer ()
  "Eval current buffer in cider."
  (interactive)
  (cider-eval-any-eval 'buffer))

(defun cider-eval-any-function ()
  "Eval current function in cider."
  (interactive)
  (cider-eval-any-eval 'function))

(defun cider-eval-any-line ()
  "Eval current line in cider."
  (interactive)
  (cider-eval-any-eval 'line))

(defun cider-eval-any-region ()
  "Eval current region in cider."
  (interactive)
  (cider-eval-any-eval 'region))

(defun cider-eval-any-eval (context)
  "Try to evaluate current CONTEXT."
  (let ((backends cider-eval-any-backends)
        backend found)
    (while (and (not found) backends)
      (setq backend (car-safe backends)
            backends (cdr-safe backends))
      (when (apply backend '(check))
        (setq found t)))
    (if backend
        (apply backend '(eval context))
      (error "Can not evaluate current %s" context))))

;;;###autoload
(define-minor-mode cider-eval-any-mode
  "Evaluate any buffer in the cider.

\\{cider-eval-any-mode-map}"
  :lighter " Cider-Any"
  :keymap cider-eval-any-mode-map)

(provide 'cider-eval-any)

;;; cider-eval-any.el ends here
