;;; cider-any.el --- Evaluate any buffer in cider.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016 by Artem Malyshev

;; Author: Artem Malyshev <proofit404@gmail.com>
;; URL: https://github.com/xquery-mode/cider-any
;; Version: 0.0.1
;; Package-Requires: ((cider "0.13.0"))

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'cider)

(defgroup cider-any nil
  "Evaluate any buffer in cider."
  :group 'cider)

(defcustom cider-any-backends nil
  "The list of active backends.

Only one backend is used at a time.  Each backend is a function
that takes a variable number of arguments.  The first argument is
the command requested from the backend.  It is one of the
following:

`check': The backend should return t in the case it can eval in
current context.  Returning nil from this command passed control
to the next backend.

`eval': Request to perform evaluation of current context.  The
second argument is the context type passed as a symbol.
`buffer', `function', `line' and `region' are default context
types.  Return value must be a string containing the clojure form
which can actually evaluate the given context.  It can contain %s
signature which will be substituted with actual evaluation
content.

`handle': Handle evaluation result.  The second argument will be
an evaluation result.  As with `handle-init' result is
represented as list of strings."
  :type '(repeat :tag "User defined" (function)))

(defvar cider-any-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'cider-any-buffer)
    (define-key map (kbd "C-M-x") 'cider-any-function)
    (define-key map (kbd "C-c C-l") 'cider-any-line)
    (define-key map (kbd "C-c C-r") 'cider-any-region)
    map)
  "Keymap for `cider-any-mode'.")

(defun cider-any-buffer ()
  "Eval current buffer in cider."
  (interactive)
  (cider-any-eval 'buffer))

(defun cider-any-function ()
  "Eval current function in cider."
  (interactive)
  (cider-any-eval 'function))

(defun cider-any-line ()
  "Eval current line in cider."
  (interactive)
  (cider-any-eval 'line))

(defun cider-any-region ()
  "Eval current region in cider."
  (interactive)
  (cider-any-eval 'region))

(defun cider-any-string (string)
  "Eval string in cider."
  (interactive "sString: ")
  (cider-any-eval string))

;; hackish without lexical let, should be improved!
(defvar cider-return-collection)
(defun cider-any-string->list (string)
  "Eval string in cider."
  (setq cider-return-collection nil)
  (let ((handler (lambda (eval-context &rest content)
                   (setq cider-return-collection (read content)))))
    (cider-any-eval string handler))
  (sleep-for 0 100) ;; ugh, cider-any-eval is asynchronous! -> BUG! -> TO FIX!
  cider-return-collection)

(defun cider-any-eval-arg (context)
  "Get eval substitution for CONTEXT."
  (replace-regexp-in-string
   "\\\""
   "\\\\\""
   (if (stringp context)
       context
     (apply
      #'buffer-substring-no-properties
      (cl-case context
        (buffer `(,(point-min)
                  ,(point-max)))
        (function `(,(save-excursion
                       (beginning-of-defun)
                       (point))
                    ,(save-excursion
                       (end-of-defun)
                       (point))))
        (line `(,(line-beginning-position)
                ,(line-end-position)))
        (region (if (not (region-active-p))
                    (error "Region is not marked")
                  `(,(region-beginning)
                    ,(region-end)))))))))

(defun cider-any-eval-handler (backend)
  "Make an interactive eval handler for BACKEND."
  (nrepl-make-response-handler
   (current-buffer)
   (lambda (buffer value)
     (with-current-buffer buffer
       (apply backend `(handle ,value))))
   (lambda (_buffer out)
     (cider-emit-interactive-eval-output out))
   (lambda (buffer err)
     (or (with-current-buffer buffer
           (apply backend `(error ,err)))
         (cider-emit-interactive-eval-err-output err)))
   '()))

(defun cider-any-eval (context &optional custom-backend)
  "Try to evaluate current CONTEXT."
  (let ((backends cider-any-backends)
        backend found)
    (while (and (not found) backends)
      (setq backend (car-safe backends)
            backends (cdr-safe backends))
      (when (apply backend '(check))
        (setq found t)))
    (if (not backend)
        (error "Can not evaluate current %s" context)
      (cider-interactive-eval
       (format
        (apply backend '(eval context))
        (cider-any-eval-arg context))
       (cider-any-eval-handler (or custom-backend backend))))))

;;;###autoload
(define-minor-mode cider-any-mode
  "Evaluate any buffer in the cider.

\\{cider-any-mode-map}"
  :lighter " Cider-Any"
  :keymap cider-any-mode-map)

(provide 'cider-any)

;;; cider-any.el ends here
