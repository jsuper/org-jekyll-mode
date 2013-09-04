
(add-to-list 'load-path (expand-file-name ".."
		  (file-name-directory (buffer-file-name))))
(require 'org-jekyll-project)

;; test org-jekyll/create-project-alist
(setq org-jekyll/jekyll-project-root "~/jekyll_demo")
(setq org-jekyll/org-mode-project-root "~/orgs")
(car (org-jekyll/create-publish-project-alist))
