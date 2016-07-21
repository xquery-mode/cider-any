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

(defvar cider-eval-any-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'cider-eval-any-buffer)
    map)
  "Keymap for `cider-eval-any-mode'.")

;;;###autoload
(define-minor-mode cider-eval-any-mode
  "Evaluate any buffer in the cider.

\\{cider-eval-any-mode-map}"
  :lighter " Cider-Any"
  :keymap cider-eval-any-mode-map)

(provide 'cider-eval-any)

;;; cider-eval-any.el ends here
