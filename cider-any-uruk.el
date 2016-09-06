;;; cider-any-uruk.el --- Evaluate XQuery with uruk in the cider  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'cider-any)
(require 'page-break-lines)

(defgroup cider-any-uruk nil
  "Evaluate XQuery documents in the cider repl."
  :group 'cider-any)

(defcustom cider-any-uruk-uri nil
  "Uri uruk/create-session argument."
  :type 'string
  :group 'cider-any-uruk)

(defcustom cider-any-uruk-user nil
  "User uruk/create-session argument."
  :type 'string
  :group 'cider-any-uruk)

(defcustom cider-any-uruk-password nil
  "Password uruk/create-session argument."
  :type 'string
  :group 'cider-any-uruk)

(defcustom cider-any-uruk-content-base nil
  "Content base uruk/create-session argument."
  :type 'string
  :group 'cider-any-uruk)

(defcustom cider-any-uruk-handler 'cider-any-uruk-display-buffer
  "Response handle function."
  :type 'function
  :group 'cider-any-uruk)

(defcustom cider-any-uruk-buffer-template "*XQuery-Result-%s*"
  "Base buffer name to show XQuery documents."
  :type 'string
  :group 'cider-any-uruk)

(defun cider-any-uruk-safe-variables-p (variables)
  (and (consp variables)
       (= 0 (% (length variables) 2))))

(defvar-local cider-any-uruk-variables nil
  "Plist of variables used in uruk query map.")

(put 'cider-any-uruk-variables 'safe-local-variable 'cider-any-uruk-safe-variables-p)

(defun cider-any-uruk-plist-to-map (plist)
  "Convert Elisp PLIST into Clojure map."
  (concat "{"
          (mapconcat #'(lambda (element)
                         (if (eq element t)
                             "true"
                           (cl-case (type-of element)
                             (cons (cider-any-uruk-plist-to-map element))
                             (integer (number-to-string element))
                             (float (number-to-string element))
                             (string (concat "\"" element "\""))
                             (symbol (symbol-name element)))))
                     plist
                     " ")
          "}"))

(defun cider-any-uruk-browse (&rest content)
  "Show CONTENT in the browser."
  (mapc
   (lambda (result)
     (let ((filename (make-temp-file "cider-any-uruk")))
       (with-temp-file filename
         (insert result))
       (browse-url (concat "file://" filename))))
   content))

(defun cider-any-uruk-display-buffer (&rest content)
  "Show CONTENT in the buffer."
  (if (not content)
      (message "Evaluation completes with on result")
    (pop-to-buffer
     (with-current-buffer
	 (get-buffer-create (format cider-any-uruk-buffer-template (buffer-name)))
       (read-only-mode -1)
       (erase-buffer)
       (insert (car content))
       (dolist (item (cdr content))
	 (insert "\n")
	 (insert (make-string 1 ?\))
	 (insert "\n")
	 (insert item))
       (goto-char (point-min))
       (normal-mode)
       (page-break-lines-mode 1)
       (read-only-mode 1)
       (local-set-key (kbd "q") 'quit-window)
       (current-buffer)))))

(defun cider-any-uruk-parse-variables-form ()
  "Clojure form for external variable search."
  "(doall (map second (re-seq
                       #\"(?m)^declare variable \\$([\\w-]+) .* external;$\"
                       \"%s\")))")

(defun cider-any-uruk-save-variables (variables)
  "Ask user for external XQuery variables values in this document."
  (dolist (variable variables)
    (unless (member variable cider-any-uruk-variables)
      (let ((value (read (read-from-minibuffer (concat variable " value: ")))))
        (setq cider-any-uruk-variables
              (append cider-any-uruk-variables
                      (list variable value)))))))

(defun cider-any-uruk-eval-form ()
  "Clojure form for XQuery document revaluation."
  (format "(let [db %s]
             (with-open [session (uruk/create-session db)]
               (doall (map str (uruk/execute-xquery
                                session
                                ;; NOTE: uruk doesn't work with nested comment literals
                                (clojure.string/replace
                                 \"%%s\"
                                 #\"(?s)\\(: Local Variables: :\\).*\\(: End: :\\)\"
                                 \"\")
                                %s)))))"
          (cider-any-uruk-plist-to-map
           `(:uri ,cider-any-uruk-uri
             :user ,cider-any-uruk-user
             :password ,cider-any-uruk-password
             :content-base ,cider-any-uruk-content-base))
          (cider-any-uruk-plist-to-map
           `(:variables ,cider-any-uruk-variables))))

(defun cider-any-uruk (command &rest args)
  "Eval XQuery in Cider.
COMMAND and ARGS stands for `cider-any' backend documentation."
  (cl-case command
    (check (eq major-mode 'xquery-mode))
    (init "(require '[uruk.core :as uruk])")
    (eval (progn
            (when current-prefix-arg
              (setq cider-any-uruk-variables nil))
            (cider-any-uruk-parse-variables-form)))
    (handle (progn
              (cider-any-uruk-save-variables (read args))
              (cons (cider-any-uruk-eval-form) 'handle-eval)))
    (handle-eval (apply cider-any-uruk-handler (read args)))))

(add-to-list 'cider-any-backends 'cider-any-uruk)

(provide 'cider-any-uruk)

;;; cider-any-uruk.el ends here
