;;; xmllint.el --- XML results pretty printer.

;;; Commentary:

;;; Code:

(require 'cider-any-uruk)

(defgroup xmllint nil
  "XML results pretty printer."
  :group 'cider-any-uruk)

(defcustom xmllint-indicator " •"
  "String for pretty print indication.")

(defvar xmllint-old-uruk-handler nil)

(defun xmllint-pprint (&rest content)
  "Pretty print xml content first."
  (let ((res (apply 'cider-any-uruk-display-buffer
                    (mapcar
                     (lambda (result)
                       (with-temp-buffer
                         (insert result)
                         (normal-mode)
                         (if (not (eq major-mode 'nxml-mode))
                             result
                           (shell-command-on-region
                            (point-min) (point-max)
                            "xmllint --format -" (buffer-name) t)
                           (indent-region (point-min) (point-max))
                           (buffer-substring-no-properties (point-min) (point-max)))))
                     content))))
    (when (bufferp res)
      (with-current-buffer res
        (setq mode-line-format
              (cons '(:propertize xmllint-indicator face bold)
                    mode-line-format))))))

;;;###autoload
(define-minor-mode xmllint-mode
  "XML results pretty printer."
  :lighter nil
  :group 'xmllint
  :global t
  (if xmllint-mode
      (setq xmllint-old-uruk-handler cider-any-uruk-handler
            cider-any-uruk-handler #'xmllint-pprint)
    (setq cider-any-uruk-handler xmllint-old-uruk-handler
          xmllint-old-uruk-handler nil)))

(provide 'xmllint)

;;; xmllint.el ends here
