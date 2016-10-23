;;; eval-any-xquery.el --- Evaluate XQuery with uruk in the cider  -*- lexical-binding: t; -*-

;; Author: Artem Malyshev <proofit404@gmail.com>
;; URL: https://github.com/xquery-mode/cider-any
;; Version: 0.0.1
;; Package-Requires: ((cider "0.13.0"))

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'cider)
(require 'eval-any)
(require 'page-break-lines)

(defgroup eval-any-xquery nil
  "Evaluate XQuery documents in the cider repl."
  :group 'eval-any)

(defun eval-any-xquery-backend (command &rest args)
  "Eval XQuery in Cider.
COMMAND and ARGS stands for `eval-any' backend documentation."
  (cl-case command
    (check (eq major-mode 'xquery-mode))
    (t (apply
        #'eval-any-xquery
        (eval-any-xquery-get-arg command)
        eval-any-xquery-handler
        nil
        args))))

(add-to-list 'eval-any-backends 'eval-any-xquery-backend)


;;; Evaluate functions.

(defcustom eval-any-xquery-connection '(:host nil :port nil :user nil :password nil :content-base nil)
  "Property list of :host :port :user :password :content-base for uruk session creation."
  :type 'plist
  :group 'eval-any-xquery)

(defun eval-any-xquery (xquery callback &optional errback &rest args)
  "Eval specified XQUERY string asynchronously.

CALLBACK function must have following signature:

    (CALLBACK RESULT &rest ARGS)

ERRBACK if specified must have following signature:

    (ERRBACK ERROR &rest ARGS)"
  (cider-ensure-connected)
  (let* ((arg (eval-any-xquery-escape xquery))
         (form (format (eval-any-xquery-get-form) arg))
         (nrepl-callback (apply #'eval-any-xquery-make-handler callback errback args))
         (connection (cider-current-connection))
         (session eval-any-xquery-session))
    (nrepl-request:eval form nrepl-callback connection session)))

(defun eval-any-xquery-sync (xquery)
  "Eval specified XQUERY string synchronously."
  (cider-ensure-connected)
  (let* ((arg (eval-any-xquery-escape xquery))
         (form (format (eval-any-xquery-get-form) arg))
         (connection (cider-current-connection))
         (session eval-any-xquery-session)
         (response (nrepl-sync-request:eval form connection session))
         (value (nrepl-dict-get response "value")))
    (and value (read value))))

(defun eval-any-xquery-escape (xquery)
  (replace-regexp-in-string "\\\"" "\\\\\"" xquery))

(defun eval-any-xquery-get-form ()
  "Clojure form for XQuery document evaluation."
  (format "(do
             (require '[uruk.core :as uruk])
             (set! *print-length* nil)
             (set! *print-level* nil)
             (let [host \"%s\"
                   port %s
                   db %s]
               (with-open [session (uruk/create-default-session (uruk/make-hosted-content-source host port db))]
                 (doall (map str (uruk/execute-xquery session \"%%s\"))))))"
          (plist-get eval-any-xquery-connection :host)
          (plist-get eval-any-xquery-connection :port)
          (eval-any-xquery-plist-to-map eval-any-xquery-connection)))

(defun eval-any-xquery-make-handler (callback errback &rest args)
  (nrepl-make-response-handler
   (current-buffer)
   (lambda (buffer value)
     (with-current-buffer buffer
       (apply callback (and value (read value)) args)))
   (lambda (_buffer out)
     (cider-emit-interactive-eval-output out))
   (lambda (buffer err)
     (with-current-buffer buffer
       (if errback
           (apply errback err args)
         (cider-emit-interactive-eval-err-output err))))
   '()))

(defun eval-any-xquery-plist-to-map (plist)
  "Convert Elisp PLIST into Clojure map."
  (concat "{"
          (mapconcat #'(lambda (element)
                         (if (eq element t)
                             "true"
                           (cl-case (type-of element)
                             (cons (eval-any-xquery-plist-to-map element))
                             (integer (number-to-string element))
                             (float (number-to-string element))
                             (string (concat "\"" element "\""))
                             (symbol (symbol-name element)))))
                     plist
                     " ")
          "}"))


;;; Interactive eval argument.

(defun eval-any-xquery-get-arg (context)
  "Get eval substitution for CONTEXT."
  (cl-case context
    (string (read-string "String: "))
    (t (apply
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


;;; Interactive eval result handlers.

(defcustom eval-any-xquery-handler 'eval-any-xquery-display-buffer
  "Response handle function."
  :type 'function
  :group 'eval-any-xquery)

(defcustom eval-any-xquery-buffer-template "*XQuery-Result-%s*"
  "Base buffer name to show XQuery documents."
  :type 'string
  :group 'eval-any-xquery)

(defun eval-any-xquery-browse (result &rest _args)
  "Show RESULT in the browser."
  (mapc
   (lambda (result)
     (let ((filename (make-temp-file "eval-any-xquery")))
       (with-temp-file filename
         (insert result))
       (browse-url (concat "file://" filename))))
   result))

(defun eval-any-xquery-display-buffer (result &rest _args)
  "Show RESULT in the buffer."
  (if (not result)
      (prog1 nil
        (message "XQuery returned an empty sequence"))
    (pop-to-buffer
     (with-current-buffer
         (get-buffer-create (format eval-any-xquery-buffer-template (buffer-name)))
       (fundamental-mode)
       (view-mode -1)
       (erase-buffer)
       (eval-any-xquery-insert result)
       (normal-mode)
       (view-mode 1)
       (page-break-lines-mode 1)
       (current-buffer)))))

(defun eval-any-xquery-insert (result)
  (let ((old-position (point)))
    (insert (car result))
    (dolist (item (cdr result))
      (insert "\n")
      (insert (make-string 1 ?\))
      (insert "\n")
      (insert item))
    (goto-char old-position)))


;;; NREPL session management.

(defvar eval-any-xquery-session nil)

(defun eval-any-xquery-connected ()
  (let ((response (nrepl-sync-request:clone (current-buffer))))
    (nrepl-dbind-response response (new-session err)
      (if new-session
          (setq eval-any-xquery-session new-session)
        (error "Could not create new session (%s)" err)))))

(defun eval-any-xquery-disconnected ()
  (setq eval-any-xquery-session nil))

(add-hook 'nrepl-connected-hook 'eval-any-xquery-connected)
(add-hook 'nrepl-disconnected-hook 'eval-any-xquery-disconnected)

(provide 'eval-any-xquery)

;;; eval-any-xquery.el ends here
