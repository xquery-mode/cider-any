;;; eval-any.el --- Evaluate anything.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016 by Artem Malyshev

;; Author: Artem Malyshev <proofit404@gmail.com>
;; URL: https://github.com/xquery-mode/cider-any
;; Version: 0.0.1

;;; Commentary:

;;; Code:

(require 'cl-lib)

(defgroup eval-any nil
  "Evaluate any buffer in cider."
  :group 'cider)

(defcustom eval-any-backends nil
  "The list of active backends.

Only one backend is used at a time.  Each backend is a function
that takes a variable number of arguments.  The first argument is
the command requested from the backend.  It is one of the
following:

`check': The backend should return t in the case it can eval in
current context.  Returning nil from this command passed control
to the next backend.

`buffer', `function', `line', `region' or `string': Request to
perform evaluation of given context."
  :type '(repeat :tag "User defined" (function)))

(defvar eval-any-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'eval-any-buffer)
    (define-key map (kbd "C-M-x") 'eval-any-function)
    (define-key map (kbd "C-c C-l") 'eval-any-line)
    (define-key map (kbd "C-c C-r") 'eval-any-region)
    (define-key map (kbd "M-:") 'eval-any-string)
    map)
  "Keymap for `eval-any-mode'.")

(defun eval-any-buffer ()
  "Eval current buffer in cider."
  (interactive)
  (eval-any 'buffer))

(defun eval-any-function ()
  "Eval current function in cider."
  (interactive)
  (eval-any 'function))

(defun eval-any-line ()
  "Eval current line in cider."
  (interactive)
  (eval-any 'line))

(defun eval-any-region ()
  "Eval current region in cider."
  (interactive)
  (eval-any 'region))

(defun eval-any-string ()
  "Eval string in cider."
  (interactive)
  (eval-any 'string))

(defun eval-any (context)
  "Try to evaluate given CONTEXT."
  (let ((backends eval-any-backends)
        backend found)
    (while (and (not found) backends)
      (setq backend (car-safe backends)
            backends (cdr-safe backends))
      (when (apply backend '(check))
        (setq found t)))
    (if (not backend)
        (error "Can not evaluate current %s" context)
      (apply backend (list context)))))

;;;###autoload
(define-minor-mode eval-any-mode
  "Evaluate anything.

\\{eval-any-mode-map}"
  :lighter " Eval-Any"
  :keymap eval-any-mode-map)

(provide 'eval-any)

;;; eval-any.el ends here
