;; Org-jekyll publishing
(require 'ox-publish)
(require 'org-jekyll-project)

(defun org-jekyll/insert-yaml-front-matter-string (section value)
  (when (and value section)
    (cond ((stringp value)
	   (insert (format "%s: %s\n" section value)))
	  ((listp value)
	   (insert (format "%s:\n" section))
	   (dolist (var value)
	     (insert (format "  - %s\n" var))
	     )))))

(defun org-jekyll/publish-org-to-html (plist filename pub-dir)
  "Org-jekyll publish function, will insert yaml front matter to export files,
   We using the keywords defined in org-mode files as tag for jekyll
  "
  (let* ((output-file (org-html-publish-to-html plist filename pub-dir))
	(file-info (org-export--get-inbuffer-options))
	(yaml-plist (org-jekyll/get-yaml-front-matter filename))
	(title (org-element-interpret-data (plist-get file-info :title))))
    
    (with-current-buffer (find-file-noselect output-file)
      (goto-char (point-min))
      (let* ((notitle nil)
	     (nolayout nil))
	(dolist (yaml-v-pair yaml-plist)
	  (let* ((yaml-name (car yaml-v-pair))
		(yaml-value (plist-get yaml-v-pair yaml-name)))
	    (when (string= yaml-name "title")
	      (setq notitle nil))
	    (when (string= yaml-name "layout")
	      (setq nolayout nil))
	    (org-jekyll/insert-yaml-front-matter-string yaml-name yaml-value)
	    )
	  )
	(insert "---\n")
	(unless notitle 
	  (goto-char (point-min))
	  (org-jekyll/insert-yaml-front-matter-string "title" title))
	(unless nolayout 
	  (goto-char (point-min))
	  (org-jekyll/insert-yaml-front-matter-string "layout" (or org-jekyll/default-post-layout "post")))
	
	(goto-char (point-min))
	(insert "---\n"))
      (save-buffer)
      (kill-buffer))))


(defun org-jekyll/publish-project ()
  (interactive)
  (org-publish "org-jekyll"))

(defun org-jekyll/publish-setting-up ()
  (dolist (pub-proj (org-jekyll/create-publish-project-alist))
    (add-to-list 'org-publish-project-alist pub-proj))
)

(provide 'org-jekyll-publish)
