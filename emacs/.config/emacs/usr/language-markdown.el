;;; language-markdown.el --- Language support for markdown

;;; Commentary:

;;

;;; Code:

(defconst rangho/md2html-template "\
<!DOCTYPE html>
<html>
  <head>
    <meta charset='utf-8' />
    <script src='https://cdn.jsdelivr.net/npm/marked/marked.min.js'></script>
    <style type='text/css'>@import url(http://fonts.googleapis.com/css?family=Vollkorn:400,400italic,700,700italic&subset=latin);h1,p{margin-top:0}ol,ul{padding-left:1.2em}body,h1 a,h1 a:hover{color:#333}a,h1 a,h1 a:hover{text-decoration:none}hr,video{margin:2em 0}img{max-width:100%%}table{width:100%%}table,td,th{padding:5px}body,html{padding:1em;margin:auto;max-width:72em;background:#fefefe}body{font:1.3em/1.4 Vollkorn,Palatino,Times;text-align:justify}h1,h2,h3{font-weight:400}h3,nav{font-style:italic}code,nav{font-size:.9em}article,footer,header,nav{width:700px;margin:0 auto}article{margin-top:4em;margin-bottom:4em;min-height:400px}footer{margin-bottom:50px}video{border:1px solid #ddd}nav{border-bottom:1px solid #ddd;padding:1em 0}nav p{margin:0}h3{margin-top:3em}p{-webkit-hypens:auto;-moz-hypens:auto;hyphens:auto}ul{list-style:square}blockquote{margin-left:1em;padding-left:1em;border-left:1px solid #ddd}code{font-family:Consolas,Menlo,Monaco,monospace,serif;background:#fff}a{color:#2484c1}a:hover{text-decoration:underline}a img{border:none}hr{color:#ddd;height:1px;border-top:1px solid #ddd;border-bottom:none;border-left:0;border-right:0}p#heart{font-size:2em;line-height:1;text-align:center;color:#ccc}.red{color:#b50000}body#index li{margin-bottom:1em}@media only screen and (max-device-width:1024px){body{font-size:120%%;line-height:1.4}}@media only screen and (max-device-width:480px){body{text-align:left}article,footer{width:auto}article{padding:0 10px}}</style>
    <title>Emacs Markdown Preview</title>
  </head>
  <body>
    <div style='display: none;' id='markdown'>%s</div>
    <div id='output'></div>
    <script>
      document.getElementById('output').innerHTML =
        marked.parse(document.getElementById('markdown').innerText);
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
  :after impatient-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "multimarkdown")
  (setq markdown-fontify-code-blocks-natively t))

;; Create helper command to start web server and impatient mode
(defun rangho/markdown-preview ()
  "Preview the current markdown file in a web browser."
  (interactive)
  (if (or (eq major-mode 'markdown-mode)
          (eq major-mode 'gfm-mode))
      (progn
        (httpd-start)
        (impatient-mode)
        (imp-set-user-filter #'rangho/md2html))
    (message "Not in a markdown buffer.")))

(provide 'language-markdown)

;;; language-markdown.el ends here
