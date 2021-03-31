;;; utility-ray.el --- Create beautiful screenshot with ray.so

(defgroup rangho/ray nil
  "Emacs integration with ray.so screenshot generator."
  :group 'applications
  :prefix "rangho/ray")

(defconst rangho/ray-baseurl "https://ray.so/"
  "URL to use when accessing ray.so service.")

(defcustom rangho/ray-colors "midnight"
  "Color scheme to use when creating image."
  :group 'rangho/ray
  :type 'string
  :options '("candy" "breeze" "midnight" "sunset"))

(defcustom rangho/ray-background t
  "Whether to create background gradation image."
  :group 'rangho/ray
  :type 'boolean)

(defcustom rangho/ray-dark-mode t
  "Whether to use dark mode like cool kids."
  :group 'rangho/ray
  :type 'boolean)

(defcustom rangho/ray-padding 64
  "Padding surrounding the code window in pixels."
  :group 'rangho/ray
  :type 'integer
  :options '(16 32 64 128))

(defun rangho/ray-get-region ()
  "Gets the content of the selected region."
  (let ((region-start-point (if (use-region-p) (region-beginning) (point-min)))
        (region-end-point (if (use-region-p) (region-end) (point-max))))
    (buffer-substring-no-properties region-start-point region-end-point)))

(defun rangho/ray-assemble-url ()
  "Creates the request url according to settings."
  (concat rangho/ray-baseurl
          "?colors=" (if rangho/ray-colors "true" "false")
          "&background=" (if rangho/ray-background "true" "false")
          "&darkMode=" (if rangho/ray-dark-mode "true" "false")
          "&padding=" (number-to-string rangho/ray-padding)
          "&title=" (buffer-name)
          "&code=" (base64-encode-string (rangho/ray-get-region))))

(defun screenshot-ray ()
  "Open current region in ray.so"
  (interactive)
  (browse-url (rangho/ray-assemble-url)))

(provide 'utility-ray)
