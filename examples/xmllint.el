;; Use xmllint to pretty print xml results.

(defun nxml-pprint-uruk (&rest content)
  "Pretty print xml content first."
  (apply 'cider-any-uruk-display-buffer
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
		(buffer-substring-no-properties (point-min) (point-max))
		(error "we are here"))))
	  content)))

(setq cider-any-uruk-handler #'nxml-pprint-uruk)
