;;; dbselector.el --- Per buffer Uruk DB connection parameters.

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'cider-any-uruk)

(defgroup dbselector nil
  "Per buffer Uruk DB connection parameters."
  :group 'cider-any-uruk)

(defvar dbselector-databases nil)

(defvar dbselector-uruk-variables
  '(cider-any-uruk-content-base
    cider-any-uruk-uri
    cider-any-uruk-user
    cider-any-uruk-password))

;;;###autoload
(defun dbselector-add (content-base uri user password)
  "Register database params."
  (interactive (list (read-string "DB: ")
                     (read-string "Uri: ")
                     (read-string "User: ")
                     (read-string "Passwd: ")))
  (push (mapcar (lambda (x) (if (string= x "") nil x))
                (list content-base uri user password))
        dbselector-databases))

;;;###autoload
(defun dbselector-remove (db)
  "Unregister database params."
  (interactive (list (completing-read "DB: " (mapcar 'car dbselector-databases))))
  (print (delete (print (assoc db dbselector-databases)) dbselector-databases)))

;;;###autoload
(defun dbselector-set (db)
  "Set Uruk database for current buffer."
  (interactive (list (completing-read "DB: " (mapcar 'car dbselector-databases))))
  (cl-mapcar 'set
             dbselector-uruk-variables
             (assoc db dbselector-databases))
  (force-mode-line-update))

;;;###autoload
(defun dbselector-unset ()
  "Unset Uruk database for current buffer."
  (interactive)
  (cl-mapcar 'set
             dbselector-uruk-variables
             (make-list 4 nil))
  (force-mode-line-update))

(defvar dbselector-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c d b a") 'dbselector-add)
    (define-key map (kbd "C-c d b r") 'dbselector-remove)
    (define-key map (kbd "C-c d b s") 'dbselector-set)
    (define-key map (kbd "C-c d b u") 'dbselector-unset)
    map)
  "Keymap for dbselector-mode.")

(defun dbselector-mode-indicator ()
  (if cider-any-uruk-uri
      (format "[%s]"
              (or cider-any-uruk-content-base
                  (format "%s/default"
                          cider-any-uruk-user)))
    ""))

;;;###autoload
(define-minor-mode dbselector-mode
  "DBSelector minor mode.

\\{dbselector-mode-map}"
  :lighter (" DB" (:eval (dbselector-mode-indicator)))
  :group 'dbselector
  :keymap dbselector-mode-map
  (if dbselector-mode
      (mapc 'make-local-variable dbselector-uruk-variables)
    (mapc 'kill-local-variable dbselector-uruk-variables)))

(add-hook 'xquery-mode-hook 'dbselector-mode)

(provide 'dbselector)

;;; dbselector.el ends here
