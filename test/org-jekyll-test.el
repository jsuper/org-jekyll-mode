
(add-to-list 'load-path (expand-file-name ".."
		  (file-name-directory (buffer-file-name))))
(require 'org-jekyll-util)
(require 'org-jekyll-publish)
(require 'org-jekyll-project)

(custom-set-variables
 '(org-jekyll/jekyll-project-root "~/org-jekyll-test/jekyll-root")
 '(org-jekyll/org-mode-project-root "~/org-jekyll-test/org-mode-root")
 '(org-jekyll/org-mode-static-files-folder-name "assets"))

(setq org-publish-project-alist (org-jekyll/create-publish-project-alist))


