;;; eval-any-xquery-to-file.el --- Associate XQuery result with file.

;;; Commentary:

;;; Code:

(require 'eval-any-xquery)

(defgroup eval-any-xquery-to-file nil
  "XML results pretty printer."
  :group 'eval-any-xquery)

(defvar eval-any-xquery-to-file-old-handler nil)

(defvar eval-any-xquery-buffer-filename nil
  "Filename for the new XQuery document buffer to be created.")

(defun eval-any-xquery-to-file-handler (result &rest args)
  (let ((res (apply eval-any-xquery-to-file-old-handler result args)))
    (when (and res eval-any-xquery-buffer-filename)
      (with-current-buffer res
        (setq buffer-file-name eval-any-xquery-buffer-filename)
        (normal-mode t)
        (rename-buffer eval-any-xquery-buffer-filename t)
        (setq eval-any-xquery-buffer-filename nil)
        (set-buffer-modified-p nil)
        (eval-any-xquery-after-normal-mode)))
    res))

;;;###autoload
(define-minor-mode eval-any-xquery-to-file-mode
  "XML results pretty printer."
  :lighter nil
  :group 'eval-any-xquery-to-file
  :global t
  (if eval-any-xquery-to-file-mode
      (setq eval-any-xquery-to-file-old-handler eval-any-xquery-handler
            eval-any-xquery-handler #'eval-any-xquery-to-file-handler)
    (setq eval-any-xquery-handler eval-any-xquery-to-file-old-handler
          eval-any-xquery-to-file-old-handler nil)))

(provide 'eval-any-xquery-to-file)

;;; eval-any-xquery-to-file.el ends here