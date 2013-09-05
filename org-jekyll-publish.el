;; Org-jekyll publishing
(require 'org-jekyll-project)

(defun org-jekyll/insert-yaml-front-matter (section value-list)
  (when value-list
    (if (= 1 (length value-list))
	(insert-string (format "%s: %s\n" section (car value-list)))
      (insert-string (format "%s:\n" section))
      (dolist (cats value-list)
	(insert-string (format " - %s\n" cats)))
      )))

(defun org-jekyll/publish-org-to-html (plist filename pub-dir)
  "Org-jekyll publish function, will insert yaml front matter to export files,
   We using the keywords defined in org-mode files as tag for jekyll
  "
  (let ((output-file (org-html-publish-to-html plist filename pub-dir))
	(file-info (org-combine-plists
		    (org-export--get-inbuffer-options)))
	(yaml-plist (org-jekyll/get-yaml-front-matter filename)))
    (princ yaml-plist)
    (with-current-buffer (find-file-noselect output-file)
      (let ((layout (if (member :layout yaml-plist)
			(getf yaml-plist :layout)
		      "default"))
	    (title (org-element-interpret-data (plist-get file-info :title))))
	(goto-char (point-min))
	(insert-string "---\n") ;;insert yaml front matter beginning
	(insert-string (format "layout: %s\n" layout))
	(insert-string (format "title: %s\n" title))
	(when (member :categories yaml-plist)
	  (org-jekyll/insert-yaml-front-matter "categories" 
					       (split-string (getf yaml-plist :categories) ";")))
	(when (member :tag yaml-plist)
	  (org-jekyll/insert-yaml-front-matter "tag"
					       (split-string (getf yaml-plist :tag) ";")))
	(insert-string "---\n")

	)
      (save-buffer)
      (kill-buffer))))

(defun org-jekyll/correct-static-files-link ())

(provide 'org-jekyll-publish)
