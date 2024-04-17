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
  (org-id-link-to-org-use-id t)
  (org-startup-indented t)
  (org-pretty-entities t)
  (org-startup-with-inline-images t))

(use-package org-superstar
  :hook (org-mode . org-superstar-mode))

(use-package org-roam
  :after org
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture))
  :custom
  (org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  :config
  (org-roam-db-autosync-mode))

(use-package org-roam-ui
  :after org-roam
  :custom
  (org-roam-ui-sync-theme t)
  (org-roam-ui-follow t)
  (org-roam-ui-update-on-save t)
  (org-roam-ui-open-on-start t))

(use-package ox-reveal
  :custom
  (org-reveal-root-path "https://cdn.jsdelivr.net/npm/reveal.js"))

(provide 'language-org)

;;; language-org.el ends here
