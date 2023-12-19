;;; language-dart.el --- Language support for Dart and Flutter

;;; Commentary:

;;

;;; Code:

(use-package dart-mode
  :hook (dart-mode . eglot-ensure)
  :init
  (add-to-list 'eglot-server-programs '(dart-mode . ("dart" "language-server"
                                                     "--client-id" "emacs.eglot"
                                                     "--client-version" "1.0.0"))))

(use-package flutter
  :after dart-mode
  :bind (:map dart-mode-map
              ("C-c C-c" . flutter-run-or-hot-reload)))

(provide 'language-dart)

;;; language-dart.el ends here
