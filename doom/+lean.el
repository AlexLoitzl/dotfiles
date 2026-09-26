(add-hook! nael-mode
  (setq-local display-fill-column-indicator-column 100)
  (display-fill-column-indicator-mode +1)
  (eglot-ensure)
  ;; Change commenting style to use single line comments
  (setq-local comment-start "--")
  (setq-local comment-end "")
  (setq eldoc-echo-area-prefer-doc-buffer t))

;;; +lean.el ends here
