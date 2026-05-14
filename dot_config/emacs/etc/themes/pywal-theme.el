;;; pywal-theme.el --- Emacs colorscheme that uses pywal-generated colors

;;; Commentary:

;;; Code:

(require 'rangho-themes)

(defgroup pywal-theme nil
  "Emacs colorscheme that uses pywal-generated colors."
  :group 'local
  :prefix "pywal-theme-")

(defcustom pywal-theme-colors-file
  (expand-file-name "wal/colors-emacs.el"
                    (or (getenv "XDG_CACHE_HOME")
                        (expand-file-name ".cache" (getenv "HOME"))))
  "Path to the pywal-generated colors.el file.")

(rangho-themes-define-theme 'pywal)

(provide-theme 'pywal)

(provide 'pywal-theme)
;;; pywal-theme.el ends here
