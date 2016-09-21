;;; dbselector.el --- Per buffer Uruk DB connection parameters.

;;; Commentary:

;;; Code:

(require 'cider-any-uruk)
(require 'cl-lib)
(require 'url-parse)

(defgroup dbselector nil
  "Per buffer Uruk DB connection parameters."
  :group 'cider-any-uruk)

(defvar dbselector-databases nil)

(defvar dbselector-uruk-variables
  '(cider-any-uruk-uri
    cider-any-uruk-user
    cider-any-uruk-password
    cider-any-uruk-content-base))

(defun dbselector-rehash ()
  "Ensure Uruk DB id uniqueness."
  (cl-flet* ((host (x) (url-host (url-generic-parse-url x)))
             (port (x) (url-port (url-generic-parse-url x)))
             (dbselector-duplicates (predicate seq)
              (let* ((col (mapcar predicate seq))
                     counters)
                (dolist (x col)
                  (if (assoc x counters)
                      (cl-incf (cdr (assoc x counters)))
                    (push (cons x 1) counters)))
                (mapcar 'car (cl-remove-if (lambda (x) (eq 1 (cdr x))) counters))))
             (same-host-for (db)
              (let* ((same-db (cl-remove-if-not
                               (lambda (x) (string= (cl-fifth x) db))
                               dbselector-databases)))
                (dbselector-duplicates
                 (lambda (x)
                   (host (cl-second x)))
                 same-db))))
    (let ((dups (dbselector-duplicates #'cl-fifth dbselector-databases)))
      (setq dbselector-databases
            (mapcar (lambda (x)
                      (cl-destructuring-bind (key uri user passwd db) x
                        (setq key
                              (cond
                               ((member db dups)
                                (if (member (host uri) (same-host-for db))
                                    (concat (host uri) ":" (number-to-string (port uri)) "/" db)
                                  (concat (host uri) "/" db)))
                               ((null db)
                                (concat (host uri) "/"))
                               (t db)))
                        (list key uri user passwd db)))
                    dbselector-databases)))))

(defun dbselector-current-id ()
  (car
   (cl-find-if
    (lambda (x)
      (equal (cdr x)
             (mapcar 'symbol-value dbselector-uruk-variables)))
    dbselector-databases)))

;;;###autoload
(defun dbselector-add (uri user password content-base)
  "Register database params."
  (interactive (list (read-string "Uri: ")
                     (read-string "User: ")
                     (read-passwd "Passwd: ")
                     (read-string "DB: ")))
  (push (mapcar (lambda (x) (if (string= x "") nil x))
                (list nil uri user password content-base))
        dbselector-databases)
  (dbselector-rehash)
  (force-mode-line-update))

;;;###autoload
(defun dbselector-remove (db)
  "Unregister database params."
  (interactive (list (completing-read "DB: " (mapcar 'car dbselector-databases))))
  (when (string= db (dbselector-current-id))
    (dbselector-unset))
  (setq dbselector-databases
        (cl-remove-if (lambda (x) (string= (car x) db))
                      dbselector-databases))
  (dbselector-rehash))

;;;###autoload
(defun dbselector-set (db)
  "Set Uruk database for current buffer."
  (interactive (list (if dbselector-databases
                         (completing-read "DB: " (mapcar 'car dbselector-databases))
                       (call-interactively 'dbselector-add)
                       (caar dbselector-databases))))
  (cl-mapcar 'set
             dbselector-uruk-variables
             (cdr (assoc db dbselector-databases)))
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
    (define-key map (kbd "C-c b a") 'dbselector-add)
    (define-key map (kbd "C-c b r") 'dbselector-remove)
    (define-key map (kbd "C-c b s") 'dbselector-set)
    (define-key map (kbd "C-c b u") 'dbselector-unset)
    map)
  "Keymap for dbselector-mode.")

(defun dbselector-mode-indicator ()
  (let ((id (dbselector-current-id)))
    (if id (format "[%s]" id) "")))

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
