
(defun concat-list (list &optional concat-char)
  "join each element in list with concat-char if it offered"
  (let ((cchar (if concat-char 
		   concat-char
		 "")))
    (setq result nil)
    (dolist (ele list)
      (if result
	  (setq result (concat result concat-char ele))
	(setq result ele)))
    result)
  )

(provide 'org-jekyll-util)
