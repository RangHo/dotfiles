;;; language-markdown.el --- Language support for markdown

;;; Commentary:

;;

;;; Code:

(defconst rangho/md2html-template "\
<!DOCTYPE html>
<html>
  <head>
    <meta charset=\"utf-8\" />
    <title>Emacs Markdown Preview</title>
    <script src=\"https://cdn.jsdelivr.net/npm/marked/marked.min.js\"></script>
    <link rel=\"stylesheet\" href=\"https://cdn.jsdelivr.net/npm/github-markdown-css/github-markdown.css\">
    <style>
      #source {
        display: none;
      }
      #result {
        box-sizing: border-box;
        min-width: 200px;
        max-width: 980px;
        margin: 0 auto;
        padding: 45px;
      }
      @media (max-width: 767px) {
        #result {
          padding: 15px;
        }
      }
  </style>
  </head>
  <body>
    <div id=\"source\">%s</div>
    <article id=\"result\" class=\"markdown-body\"></div>
    <script>
      document.getElementById(\"result\").innerHTML = marked.parse(
        document.getElementById(\"source\").innerText,
        { gfm: true, silent: true },
      );
    </script>
  </body>
</html>
"
  "Template to use when rendering markdown document.")

(defun rangho/md2html (buffer)
  "Transform a markdown document in BUFFER to a self-parsing HTML file."
  (princ (with-current-buffer buffer
           (format rangho/md2html-template
                   (rangho/escape-html
                    (buffer-substring-no-properties (point-min)
                                                    (point-max)))))
         (current-buffer)))

(defun rangho/escape-html (string)
  "Replace troublesome characters in STRING with HTML entities."
  (replace-regexp-in-string
   "[&<>]"
   (lambda (m)
     (pcase m
       ("&" "&amp;")
       ("<" "&lt;")
       (">" "&gt;")))
   string))

(use-package impatient-mode)

(use-package markdown-mode
  :after (impatient-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :bind (("C-c C-c P" . markdown-impatient-preview-mode))
  :custom
  (markdown-fontify-code-block-natively t))

(define-minor-mode markdown-impatient-preview-mode
  "Toggle real-time markdown preview for a specific markdown file."
  :init-value nil
  (if markdown-impatient-preview-mode
      (progn
        (httpd-start)
        (impatient-mode 1)
        (imp-set-user-filter #'rangho/md2html))
    (imp-remove-user-filter)
    (impatient-mode -1)
    (httpd-stop)))

(provide 'language-markdown)

;;; language-markdown.el ends here
