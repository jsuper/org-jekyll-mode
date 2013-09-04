; Org-jekyll project configure directory

(defgroup org-jekyll nil
  "Write jekyll blog with org-mode")

(defcustom org-jekyll/org-mode-project-root nil
  "Define the org-mode project base path"
  :type 'string
  :group 'org-jekyll)

(defcustom org-jekyll/jekyll-project-root nil
  "Define the publishing directory for org-mode project"
  :type 'string
  :group 'org-jekyll)

(defcustom org-jekyll/export-with-toc nil
  "Define whether export the table of contents or not"
  :type 'boolean
  :group 'org-jekyll
  :options '(nil t))

(defcustom org-jekyll/headlines-level 2
  "define the headline level for export toc"
  :type 'integer
  :group 'org-jekyll)

(defcustom org-jekyll/org-mode-static-extensions
  '("css" "js" "png" "jpg" "gif" "pdf" "mp3" "swf" "zip" "gz" "txt" "el")
  "Define the file's extension which need to handle as static files"
  :type 'list
  :group 'org-jekyll)

(defcustom org-jekyll/default-post-layout "post"
  "Define the default layout for jekyll post"
  :type 'string 
  :group 'org-jekyll)

(defcustom org-jekyll/yaml-front-matter-keywords '("categories" "tag" "layout")
  "Define the yamal keywords for jekyll post. You should define it in the begining of Org-mode files and with prefix #+YAML/ plus uppercase of current"
  :type 'list
  :group 'org-jekyll)

(defun org-jekyll/create-publish-project-alist ()
  "Create the project alist for exporting org-mode file to jekyll post"
  (let ((org-mode-project-root (if org-jekyll/org-mode-project-root
				   org-jekyll/org-mode-project-root
				 (error "Project base directory should not be empty")))
	(jekyll-project-root (if org-jekyll/jekyll-project-root
				 org-jekyll/jekyll-project-root
			       (error "Project publishing directory should not be empty"))))
    (setq org-jekyll-base (append '("org-jekyll-base"			    
				    :body-only t
				    :base-extension "org"
				    :html-extension "html"
				    :recursive t
				    :publishing-function org-html-publish-to-html
				    :auto-sitemap nil
				    :section-number nil
				    :auto-preamble nil
				    :auto-postamble nil)
				  (list :base-directory (expand-file-name org-mode-project-root)
					:publishing-directory (expand-file-name "_post" jekyll-project-root)
					:with-toc org-jekyll/export-with-toc)))
    (setq org-jekyll-static (append '("org-jekyll-static"
				      :recursive t
				      :publishing-function org-publish-attachment
				      )
				    (list 
				     :publishing-directory (expand-file-name jekyll-project-root "/assets")
				     :base-directory (expand-file-name org-mode-project-root)
				     :base-extension (let ((result nil))
						       (dolist (var org-jekyll/org-mode-static-extensions)
							 (if result
							     (setq result (concat result "\\|" (format "%s" var)))
							   (setq result (format "%s" var))
							   ))
						       result))
				    ))
    (list  
     org-jekyll-base
     org-jekyll-static
     '("org-jekyll"
       :components ("org-jekyll-base" "org-jekyll-static")
       :author "org-jekyll"
       ))
    ))

(defun trim-string (string)
  "Remove white spaces in beginning and ending of STRING.
White space here is any of: space, tab, emacs newline (line feed, ASCII 10)."
  (replace-regexp-in-string "\\`[ \t\n]*" "" (replace-regexp-in-string "[ \t\n]*\\'" "" string)))

(defun org-jekyll/get-yaml-front-matter (infile)
  "Getting the pre-defined attribute from org-mode files
each yaml front matter in org-mode should be started with #+YAML/
eg:
#+YAML/layout: page
#+YAML/tag: tag1;tag2;tag3 (each tag should be seperate by ;)
#+YAML/categories: cat1;cat2;cat3 (each category should be seperate by ;)
"
  (let ((visitingp (find-buffer-visiting infile))
	(prefix "#+YAML/")
	(regexps (format "^#\\+YAML/%s:" 
			 (regexp-opt (mapcar 'upcase 
					     org-jekyll/yaml-front-matter-keywords)))))
    (setq rlist '())
    (when (not visitingp)
      (setq visitingp (find-file-noselect infile)))
    (with-current-buffer visitingp
      (goto-char (point-min))
      (while (search-forward-regexp regexps nil t)
	(save-excursion
	  (let ((last-point (point))
		(value (trim-string (buffer-substring-no-properties (point) (line-end-position)))))
	    (when (search-backward prefix nil t)
	      (let ((tele (downcase (buffer-substring (+ (length prefix) (point)) 
						      (- last-point 1)))))
		
		(when (member tele org-jekyll/yaml-front-matter-keywords)
		  (message tele)
		  (setq rlist (plist-put rlist (intern (format ":%s" tele)) value)))))))))
    rlist))


(provide 'org-jekyll-project)





