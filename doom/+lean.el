(add-hook! nael-mode
  (setq-local display-fill-column-indicator-column 100)
  (display-fill-column-indicator-mode +1)
  (eglot-ensure))
