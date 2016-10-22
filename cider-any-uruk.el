;;; cider-any-uruk.el --- Evaluate XQuery with uruk in the cider  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'cider-any)
(require 'page-break-lines)

(defgroup cider-any-uruk nil
  "Evaluate XQuery documents in the cider repl."
  :group 'cider-any)

(defcustom cider-any-uruk-connection '(:host nil :port nil :user nil :password nil :content-base nil)
  "Property list of :host :port :user :password :content-base for uruk session creation"
  :type 'plist
  :group 'cider-any-uruk)

(defcustom cider-any-uruk-handler 'cider-any-uruk-display-buffer
  "Response handle function."
  :type 'function
  :group 'cider-any-uruk)

(defcustom cider-any-uruk-buffer-template "*XQuery-Result-%s*"
  "Base buffer name to show XQuery documents."
  :type 'string
  :group 'cider-any-uruk)

(defvar cider-any-uruk-buffer-filename
  nil
  "Filename for the new XQuery document buffer to be created.")

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

(defun cider-any-uruk-set-file-name (file-name)
  ;; Don't use (set-visited-file-name file-name) anymore,
  ;; as the file-name might contain a path that might not exist locally
  ;; and set-visited-file-name also
  ;;  - changes default-directory acc. to the path. and that has side effects
  ;;  - it usually sets buffer-auto-save-file-name as well, and that leads
  ;;    to problems for non-existing paths
  (setq buffer-file-name file-name)
  (normal-mode t) ;; t means: normal-mode detection as done in find-file
  (rename-buffer file-name t)) ;; t means: make buffer name unique if not so already

(defun cider-any-uruk-display-buffer (&rest content)
  "Show CONTENT in the buffer."
  (if (not content)
      (message "XQuery returned an empty sequence")
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
       (if cider-any-uruk-buffer-filename
           (progn
             (cider-any-uruk-set-file-name cider-any-uruk-buffer-filename)
             (setq cider-any-uruk-buffer-filename nil))
         ;; these settings only if we haven't set a file-name
         (normal-mode)
         (page-break-lines-mode 1)
         (read-only-mode 1)
         ;; local-set-key actually changes the local map which is
         ;; shared with all other buffers in the same major map.
         ;; Therefore, copy current keymap so that we really set to a
         ;; *new* buffer local keymap, see
         ;; https://www.emacswiki.org/emacs/BufferLocalKeys
         (let ((local-map (current-local-map)))
           (when local-map ;; check first if there really is a local-map
             (use-local-map (copy-keymap local-map))))
         (local-set-key (kbd "q") 'quit-window))
       (set-buffer-modified-p nil)
       (current-buffer)))))

(defun cider-any-uruk-eval-form ()
  "Clojure form for XQuery document revaluation."
  (format "(do
             (require '[uruk.core :as uruk])
             (set! *print-length* nil)
             (set! *print-level* nil)
             (let [host \"%s\"
                   port %s
                   db %s]
               (with-open [session (uruk/create-default-session (uruk/make-hosted-content-source host port db))]
                 (doall (map str (uruk/execute-xquery session \"%%s\"))))))"
          (plist-get cider-any-uruk-connection :host)
          (plist-get cider-any-uruk-connection :port)
          (cider-any-uruk-plist-to-map cider-any-uruk-connection)))

(defvar cider-any-uruk-session nil)

(defun cider-any-uruk-connected ()
  (let ((response (nrepl-sync-request:clone (current-buffer))))
    (nrepl-dbind-response response (new-session err)
      (if new-session
          (setq cider-any-uruk-session new-session)
        (error "Could not create new session (%s)" err)))))

(defun cider-any-uruk-disconnected ()
  (setq cider-any-uruk-session nil))

(add-hook 'nrepl-connected-hook 'cider-any-uruk-connected)
(add-hook 'nrepl-disconnected-hook 'cider-any-uruk-disconnected)

(defun cider-any-uruk (command &rest args)
  "Eval XQuery in Cider.
COMMAND and ARGS stands for `cider-any' backend documentation."
  (cl-case command
    (check (eq major-mode 'xquery-mode))
    (session cider-any-uruk-session)
    (eval (cider-any-uruk-eval-form))
    (handle (apply cider-any-uruk-handler (read args)))))

(add-to-list 'cider-any-backends 'cider-any-uruk)

(provide 'cider-any-uruk)

;;; cider-any-uruk.el ends here
