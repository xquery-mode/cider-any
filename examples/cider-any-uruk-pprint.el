;;; cider-any-uruk-pprint.el --- XML results pretty printer.

;;; Commentary:

;;; Code:

(require 'cider-any-uruk)

(defgroup cider-any-uruk-pprint nil
  "XML results pretty printer."
  :group 'cider-any-uruk)

(defcustom cider-any-uruk-pprint-indicator " •"
  "String for pretty print indication.")

(defvar cider-any-uruk-pprint-old-handler nil)

(defun cider-any-uruk-do-pprint (&rest content)
  "Pretty print xml content first."
  (let ((res (apply 'cider-any-uruk-display-buffer
                    (mapcar
                     (lambda (result)
                       (with-temp-buffer
                         (insert result)
                         (goto-char (point-min))
                         (if (and (looking-at-p "[[:space:]]*<")
                                  (zerop
                                   (shell-command-on-region
                                    (point-min)
                                    (point-max)
                                    "xmllint --format -"
                                    (current-buffer)
                                    t)))
                             (progn
                               (nxml-mode)
                               (indent-region (point-min) (point-max))
                               (buffer-substring-no-properties (point-min) (point-max)))
                           result)))
                     content))))
    (when (bufferp res)
      (with-current-buffer res
        (setq mode-line-format
              (cons '(:propertize cider-any-uruk-pprint-indicator face bold)
                    mode-line-format))))))

;;;###autoload
(define-minor-mode cider-any-uruk-pprint-mode
  "XML results pretty printer."
  :lighter nil
  :group 'cider-any-uruk-pprint
  :global t
  (if cider-any-uruk-pprint-mode
      (setq cider-any-uruk-pprint-old-handler cider-any-uruk-handler
            cider-any-uruk-handler #'cider-any-uruk-do-pprint)
    (setq cider-any-uruk-handler cider-any-uruk-pprint-old-handler
          cider-any-uruk-pprint-old-handler nil)))

(provide 'cider-any-uruk-pprint)

;;; cider-any-uruk-pprint.el ends here
