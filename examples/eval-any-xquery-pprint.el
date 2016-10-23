;;; eval-any-xquery-pprint.el --- XML results pretty printer.

;;; Commentary:

;;; Code:

(require 'eval-any-xquery)

(defgroup eval-any-xquery-pprint nil
  "XML results pretty printer."
  :group 'eval-any-xquery)

(defcustom eval-any-xquery-pprint-indicator " •"
  "String for pretty print indication.")

(defvar eval-any-xquery-pprint-old-handler nil)

(defun eval-any-xquery-do-pprint (result &rest args)
  "Pretty print xml content first.

See `eval-any-xquery' callback definition for RESULT and ARGS
meaning."
  (let ((res (apply 'eval-any-xquery-display-buffer
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
                     result)
                    args)))
    (when (bufferp res)
      (with-current-buffer res
        (setq mode-line-format
              (cons '(:propertize eval-any-xquery-pprint-indicator face bold)
                    mode-line-format))))))

;;;###autoload
(define-minor-mode eval-any-xquery-pprint-mode
  "XML results pretty printer."
  :lighter nil
  :group 'eval-any-xquery-pprint
  :global t
  (if eval-any-xquery-pprint-mode
      (setq eval-any-xquery-pprint-old-handler eval-any-xquery-handler
            eval-any-xquery-handler #'eval-any-xquery-do-pprint)
    (setq eval-any-xquery-handler eval-any-xquery-pprint-old-handler
          eval-any-xquery-pprint-old-handler nil)))

(provide 'eval-any-xquery-pprint)

;;; eval-any-xquery-pprint.el ends here
