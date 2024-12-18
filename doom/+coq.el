;; Emojis
(add-hook 'coq-mode-hook
          (lambda ()
            (setq-local prettify-symbols-alist
                        '(("Qed." . ?🐣) ("Defined." . ?🐤) ("Admitted." . ?🍗)))))

;; M-ö, M-ä: Enable Jumping to definitions in evil mode
(after! coq-mode
  (map! :map coq-mode-map
        "M-ö" #'company-coq-jump-to-definition
        "M-ä" #'pop-global-mark
		"M-ü" (cmd! (setq proof-three-window-mode-policy 'hybrid) (proof-layout-windows))))

;; set after package doesn't work :(
;; Configure when to change layout modes of proof general (1 column vs. 2 columns)
(setq split-width-threshold 190)
