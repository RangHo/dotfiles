;;; language-markdown.el --- Language support for markdown

;; Markdown "live preview" support with impatient mode
(defconst rangho/md2html-template "\
<!DOCTYPE html>
<html>
  <head>
    <meta charset='utf-8' />
    <script src='https://cdn.jsdelivr.net/npm/marked/marked.min.js'></script>
    <title>Emacs Markdown Preview</title>
  </head>
  <body>
    <div style='display: none;' id='markdown'>%s</div>
    <div id='output'></div>
    <script>
      document.getElementById('output').innerHTML =
        marked(document.getElementById('markdown').innerText);
    </script>
  </body>
</html>
"
  "Template to use when rendering markdown document.")
(defun rangho/md2html (buffer)
  (princ (with-current-buffer buffer
           (format md2html-template (buffer-substring-no-properties
                                     (point-min)
                                     (point-max))))
         (current-buffer)))

(use-package impatient-mode)

;; Install markdown major mode
(defun rangho/markdown-hook ()
  (imp-set-user-filter 'md2html))

(use-package markdown-mode
  :after impatient-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :hook ((markdown-mode . rangho/markdown-hook)
         (gfm-mode . rangho/markdown-hook))
  :init
  (setq markdown-command "multimarkdown")
  (setq markdown-fontify-code-blocks-natively t)
  :commands (markdown-mode gfm-mode))

(provide 'language-markdown)
