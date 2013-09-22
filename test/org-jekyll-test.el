
(add-to-list 'load-path (expand-file-name ".."
		  (file-name-directory (buffer-file-name))))

(custom-set-variables
 '(org-jekyll/jekyll-project-root "~/org-jekyll-test/jekyll-root")
 '(org-jekyll/org-mode-project-root "~/org-jekyll-test/org-mode-root")
 '(org-jekyll/org-mode-static-files-folder-name "assets"))

(require 'org-jekyll-mode)

