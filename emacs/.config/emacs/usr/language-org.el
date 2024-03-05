;;; language-org.el --- Language support for Org Mode

;;; Commentary:

;;

;;; Code:

(use-package org
  :bind (("C-c a" . org-agenda)
         ("C-c t" . org-capture))
  :custom
  (org-directory "~/Documents/Notes")
  (org-default-notes-file (concat org-directory "/default.org"))
  (org-id-link-to-org-use-id t))

(use-package org-roam
  :after org
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture))
  :config
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode))

(provide 'language-org)

;;; language-org.el ends here
