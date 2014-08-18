;; org-jekyll-mode -- is a minor mode, which help to export org-mode file
;; as an jekyll-formatted html.
;;
;;
;; Copyright (C) 2012-2014 Tang Ling
;;
;; Author: Tang Ling (http://jsuper.github.io)
;; URL: http://github.com/jsuper/org-jekyll-mode
;; GIT: http://github.com/jsuper/org-jekyll-mode
;;
;; This file is NOT part of GNU Emacs.
;;
;; Install
;;     Please see the README.md file from the same distribution


(defgroup org-jekyll nil
  "Org-mode for jekyll project")

(defcustom org-jekyll/org-mode-project-root nil
  "Define the org-mode project base path, 
you must specify it before you invoke org-jekyll/publish-setting-up"
  :type 'string
  :group 'org-jekyll)

(defcustom org-jekyll/jekyll-project-root nil
  "Define the publishing directory for org-mode project,
 you must specify it before you invoke org-jekyll/publish-setting-up"
  :type 'string
  :group 'org-jekyll)

(defcustom org-jekyll/export-with-toc nil
  "Define whether export the Table of Contents or not
nil or t"
  :type 'boolean
  :group 'org-jekyll
  :options '(nil t))

(defcustom org-jekyll/headlines-level 2
  "Define the headline level for export toc"
  :group 'org-jekyll
  :type 'integer)

(defcustom  org-jekyll/org-mode-static-files-folder-name nil
  "Define the folder name in org-mode-project root which used to 
store static files for org-mode"
  :type 'string
  :group 'org-jekyll)

(defcustom org-jekyll/jekyll-static-folder-name nil
  "The target folder to store static files, i.e. image, css, attachment
in jekyll project"
  :type 'string
  :group 'org-jekyll)

(defcustom org-jekyll/default-post-layout "post"
  "Define the default layout for jekyll post"
  :type 'string
  :group 'org-jekyll)

(defcustom org-jekyll/html-link-home "/"
  "define the html link root path:
e.g: If your jekyll project root is $HOST/jekyll,
then set this variable to /jekyll
"
  :type 'string
  :group 'org-jekyll)

(defcustom org-jekyll/yaml-list-value-sperator ";"
  "Yaml values seprator"
  :type 'string
  :group 'org-jekyll)

(defvar org-jekyll/org-mode-static-extensions
  '("css" "js" "png" "jpg" "gif" "pdf" "mp3" "swf" "zip" "gz" "txt" "el")
  "Define the file's extension which need to handle as static files")

(defvar org-jekyll/yaml-front-matter-keywords 
  '(layout "string"
	   title "string"
	   permalink "string"
	   published "string"
	   category "string"
	   categories "list"
	   tags "list")
  "Define the jekyll yaml front matter and its value type, 
if the value type is list, then it's value will be use org-jekyll/yaml-list-value-sperator
to seprate each one. To add new yaml keyword, do like this:
(add-to-list 'org-jekyll/yaml-front-matter-keywords '(newk \"valutype\") ")

(defvar org-jekyll/project-alist-inited nil
  "Let org-jekyll to lazy initial the project-alist
Please don't change this var")

(defun trim-string (string)
  "Remove white spaces in beginning and ending of STRING.
White space here is any of: space, tab, emacs newline (line feed, ASCII 10)."
  (replace-regexp-in-string "\\`[ \t\n]*" "" 
			    (replace-regexp-in-string "[ \t\n]*\\'" 
						      "" 
						      string)))

(defun concat-list (list &optional concat-char)
  "join each element in LIST with CONCAT-CHAR if it offered"
  (let ((cchar (if concat-char 
		   concat-char
		 "")))
    (setq result nil)
    (dolist (ele list)
      (if result
	  (setq result (concat result concat-char ele))
	(setq result ele)))
    result))

(defun org-jekyll/create-publish-project-alist ()
  "Create the project alist for exporting org-mode file to jekyll post"
  (unless org-jekyll/jekyll-project-root
    (error "Project base directory should not be empty"))
  (unless org-jekyll/org-mode-project-root
    (error "Project publishing directory should not be empty"))
  `(("org-jekyll-base"
     :body-only t
     :base-extension "org"
     :html-extension "html"
     :html-link-use-abs-url t
     :recursive t
     :publishing-function org-jekyll/publish-org-to-html
     :auto-sitemap nil
     :auto-preamble nil
     :auto-postamble nil
     :headline-levels ,org-jekyll/headlines-level
     :base-directory ,(expand-file-name org-jekyll/org-mode-project-root)
     :html-link-home ,org-jekyll/html-link-home
     :publishing-directory ,(expand-file-name "_posts"
					      org-jekyll/jekyll-project-root)
     :with-toc ,org-jekyll/export-with-toc)
    ("org-jekyll-static"
     :recursive t
     :publishing-function org-publish-attachment
     :publishing-directory ,(expand-file-name org-jekyll/jekyll-static-folder-name
                                              org-jekyll/jekyll-project-root)
     :base-directory ,(expand-file-name 
		       org-jekyll/org-mode-static-files-folder-name
		       org-jekyll/org-mode-project-root)
     :base-extension ,(let ((result nil))
		       (dolist 
			   (var org-jekyll/org-mode-static-extensions)
			 (if result
			     (setq result (concat result 
						  "\\|" 
						  (format "%s" var)))
			   (setq result (format "%s" var))
			   ))
		       result))
    ("org-jekyll"
       :components ("org-jekyll-base" "org-jekyll-static")
       :author "org-jekyll"
       )))

(defun org-jekyll/get-yaml-front-matter (infile)
  "Getting the pre-defined attribute from INFILE
each yaml front matter in org-mode should be started with #+YAML/
eg:
#+YAML/layout: page
#+YAML/tag: tag1;tag2;tag3 (each tag should be seperate by org-jekyll/yaml-list-value-sperator)
#+YAML/categories: cat1;cat2;cat3 (each category should be seperate by org-jekyll/yaml-list-value-sperator)
"
  (let ((visitingp (find-buffer-visiting infile))
	(regexp "^#\\+YAML/\\(.*\\):\\(.*\\)")
	(result))
    (when (not visitingp)
      (setq visitingp (find-file-noselect infile)))
    (with-current-buffer visitingp
      (goto-char (point-min))
      (while (search-forward-regexp regexp nil t)
	(let* ((yaml-name (match-string-no-properties 1))
	       (yaml-value (match-string-no-properties 2))
	       (definedp (plist-get 
			  org-jekyll/yaml-front-matter-keywords 
			  (intern (downcase yaml-name)))))
	  (when (and yaml-name yaml-value)
	    (if definedp
            (add-to-list 'result 
			 (list (downcase yaml-name)
			       (cond ((string= definedp "string")
				      (trim-string yaml-value))
				     ((string= definedp "list")
				      (split-string (trim-string yaml-value) 
						    org-jekyll/yaml-list-value-sperator)))))
          ;;if not defined in `org-jekyll/yaml-front-matter-list`, then treat it as string type
          (add-to-list 'result (list (downcase yaml-name)
                                     yaml-value))))))) 
    result))

;;;###autoload
(defun org-jekyll/new-post (&optional with-date-prefix)
  "Create an org-mode file in org-jekyll/org-mode-project-root.
If you want to create the post in a new sub directory, you can 
use / to seprate the subdirectory and file name. e.g: I want to
create a file named hello-world and put it in subdirectory '2013',
then I will input like below:
2013/hello-world

If you don't setting the org-jekyll/org-mode-project-root, then
it will failed.
"
  (interactive)
  (unless org-jekyll/org-mode-project-root
    (error "You never define the org-jekyll/org-mode-project-root"))
  
  (let* ((paths (split-string (read-string (if with-date-prefix
					       "Post Title(Begin with yyyy-mm-dd):"
					     "Post Title:")) "/"))
	 (pdirs (butlast paths))
	 (file-name (if with-date-prefix
			(format "%s-%s.org"
				(format-time-string "%Y-%m-%d")
				(car (last paths)))
		      (format "%s.org" (car (last paths)))))
	 (pdirpath (if (and pdirs (>= (length pdirs) 1))
		       (concat-list pdirs "/")
		     nil))
	 (file-name-path (if pdirpath 
			     (concat pdirpath "/" file-name)
			   file-name))
	 (pdir-abs-path (if pdirpath
			    (expand-file-name pdirpath
					      org-jekyll/org-mode-project-root)
			  nil)))
    (when pdir-abs-path
      (unless (file-exists-p pdir-abs-path)
	(make-directory pdir-abs-path t)))
    (message "Create Post [%s]" file-name-path)
    (find-file (expand-file-name file-name-path
				 org-jekyll/org-mode-project-root))
    (message "Post [%s] has been created" file-name-path)))

;(defun org-jekyll/handle-image-link-before-processing (back-end)
;  (case back-end
;    ('html 
;     (while (search-forward-regexp "\\[\\[.*\\.\\(?:png\\|gif\\)\\]\\]" nil t)
;      (let* ((matched-str (match-string-no-properties 0))	  
;	   (file-paths (substring matched-str 2 (- (length matched-str) 2)))
;	   (relaced-str (format "[[oj-img:%s]]" (substring 
;					  (expand-file-name (concat org-jekyll/org-mode-project-root "/" file-paths))
;					  (length (expand-file-name org-jekyll/org-mode-project-root))
;					  ))))
;	(replace-match relaced-str nil t)
;	)))))
;
;(defun org-jekyll/custom-org-link-type-exporter (path desc format)
;;  (print (org-export--get-subtree-options))
;  (cond 
;   ((eq format 'html)
;    (format "<img src=\"%s\" alt=\"%s\"/>" path desc))))
;
;(add-hook 'org-export-before-parsing-hook 'org-jekyll/handle-image-link-before-processing)
;(org-add-link-type "oj-img" nil 'org-jekyll/custom-org-link-type-exporter)

(defun org-jekyll/insert-yaml-front-matter-string (section value)
  (when (and value section)
    (cond ((stringp value)
	   (insert (format "%s: %s\n" section value)))
	  ((listp value)
	   (insert (format "%s:\n" section))
	   (dolist (var value)
	     (insert (format "  - %s\n" var)))))))

(defun normalize-org-mode-date-options (org-date-raw)
  "The date option which inserted by org-mode has more format,
will return yyyy-mm-dd format if exists"
  (and (string-match "\\([0-9]\\{4\\}[-\\|/][0-9]\\{1,2\\}[-\\|/][0-9]\\{1,2\\}\\)" 
		     org-date-raw)
       (replace-regexp-in-string "/" "-" (match-string 1 org-date-raw))))


(defun org-jekyll/publish-org-to-html (plist filename pub-dir)
  "Org-jekyll publish function, will insert yaml front matter to export files,
   We using the keywords defined in org-mode files as tag for jekyll
  "
  (let* ((output-file (org-html-publish-to-html plist 
						filename
						pub-dir))
	 (output-file-name (file-name-nondirectory output-file))
	 (output-file-parent-path (file-name-directory output-file))
	 (file-name-normalize-p (string-match-p "^[0-9]\\{4\\}-[0-9]\\{1,2\\}-[0-9]\\{1,2\\}.*" 
						output-file-name))
	 (file-info (org-export--get-inbuffer-options))
	 (yaml-plist (org-jekyll/get-yaml-front-matter filename))
	 (title (org-element-interpret-data (plist-get file-info :title)))
	 (date (org-element-interpret-data (plist-get file-info :date)))
	 (date-str (or (normalize-org-mode-date-options date)
		       (format-time-string "%Y-%m-%d")))
         (rootp (or (car (last (split-string org-jekyll/jekyll-project-root "[\\/]")))
                    "")))
    (with-current-buffer (find-file-noselect output-file)
      (goto-char (point-min))
      (let* ((notitle nil)
	     (nolayout nil))
	(dolist (yaml-v-pair yaml-plist)
	  (let* ((yaml-name (car yaml-v-pair))
		(yaml-value (plist-get yaml-v-pair yaml-name)))
	    (when (string= yaml-name "title")
	      (setq notitle t))
	    (when (string= yaml-name "layout")
	      (setq nolayout t))
	    (org-jekyll/insert-yaml-front-matter-string yaml-name yaml-value)))
	(insert "---\n")
	(unless notitle 
	  (goto-char (point-min))
	  (org-jekyll/insert-yaml-front-matter-string "title" title))
	(unless nolayout 
	  (goto-char (point-min))
	  (org-jekyll/insert-yaml-front-matter-string
	   "layout" 
	   (or org-jekyll/default-post-layout "post")))
	(goto-char (point-min))
	(insert "---\n"))
      (goto-char (point-min))
      (while (search-forward-regexp "\\(<img.*src=\"\\)\\(.*?\\)\\(\".*?>\\)" nil t)
        (let* ((srcp (match-string-no-properties 2)))
          (replace-match (concat "\\1/" rootp (file-truename srcp) "\\3"))))
      (save-buffer)
      (kill-buffer))
    (unless file-name-normalize-p
      (when date-str
	(let* ((new-name (concat date-str "-" output-file-name))
	       (new-path (expand-file-name new-name
					   output-file-parent-path)))
	  (if (file-exists-p new-path)
	      (delete-file new-path))
	  (rename-file output-file new-path))))))

;;;###autoload
(defun org-jekyll/publish-project ()
  (interactive)
  (unless org-jekyll/project-alist-inited
    (when (not (boundp 'org-publish-project-alist))
      (require 'ox-publish))
    (dolist (pub-proj (org-jekyll/create-publish-project-alist))
      (add-to-list 'org-publish-project-alist pub-proj))
    (setq org-jekyll/project-alist-inited t))
  (org-publish "org-jekyll"))

;;;###autoload
(define-minor-mode org-jekyll-mode
  "An emacs minor mode for creating and publishing org-mode files to jekyll standard post"
  nil
  ""
  `(
    (,(kbd "C-c C-n") . org-jekyll/new-post)
    (,(kbd "C-c C-d") . (lambda ()
			  (interactive)
			  (org-jekyll/new-post t)))
    (,(kbd "C-c C-p") . org-jekyll/publish-project)))

(provide 'org-jekyll-mode)
;;---------------------------------------
;; org-jekyll-mode end here
