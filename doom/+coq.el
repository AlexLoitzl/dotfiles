;; M-ö, M-ä: Enable Jumping to definitions in evil mode
(after! coq-mode
    (map! :map coq-mode-map
          "M-ö" #'company-coq-jump-to-definition
          "M-ä" #'pop-global-mark
          "M-ü" (cmd! (setq proof-three-window-mode-policy 'hybrid) (proof-layout-windows)))
    (map! :map coq-goals-mode-map
          "M-ö" #'company-coq-jump-to-definition
          "M-ä" #'pop-global-mark)
    (map! :map coq-response-mode-map
          "M-ö" #'company-coq-jump-to-definition
          "M-ä" #'pop-global-mark))


;; set after package doesn't work :(
;; Configure when to change layout modes of proof general (1 column vs. 2 columns)
;; (setq split-width-threshold 190)

;; Disable Company Coq Features (Including prettify-symbols)
(after! company-coq
  (add-to-list  'company-coq-disabled-features 'prettify-symbols))

;; ****************************************************
;; ********************* IRIS SYM *********************
;; ****************************************************

(defun iris-input-config ()
  "Set up math input for Iris.
    Based on https://gitlab.mpi-sws.org/iris/iris/-/blob/master/docs/editor.md, and
    https://github.com/tchajed/dotfiles/blob/master/emacs/doom/%2Bcoq.el"

  ;; Input method for the minibuffer
  (defun my-inherit-input-method ()
    "Inherit input method from `minibuffer-selected-window'."
    (let* ((win (minibuffer-selected-window))
           (buf (and win (window-buffer win))))
      (when buf
        (activate-input-method (buffer-local-value 'current-input-method buf)))))
  (add-hook 'minibuffer-setup-hook #'my-inherit-input-method)
  ;; Define the actual input method
  (quail-define-package "math" "UTF-8" "Ω" t)

  ;; ;; https://emacs.stackexchange.com/questions/76725/how-to-implement-a-function-in-quail-define-rules-for-set-input-method
  ;; (defun quail-action-f (key idx)    ; key=keyword, idx=length
  ;;   (quail-delete-region)                 ; these lines apparently needed
  ;;   (setq quail-current-str nil           ; to remove key.
  ;;         quail-converting nil            ; (not sure why all 4 is needed)
  ;;         quail-conversion-str "")        ;


  ;;   (insert "⟨⟩")
  ;;   (forward-char -1)

  ;;   (throw 'quail-tag nil)                ; this is need for finishing up?
  ;;   )

  (quail-define-rules ; add whatever extra rules you want to define here...
    ("\\fun"    ?λ)
    ("\\mult"   ?⋅)
    ("\\ent"    ?⊢)
    ("\\valid"  ?✓)
    ("\\diamond" ?◇)
    ("\\box"    ?□)
    ("\\bbox"   ?■)
    ("\\later"  ?▷)
    ("\\pred"   ?φ)
    ("\\and"    ?∧)
    ("\\or"     ?∨)
    ("\\comp"   ?∘)
    ("\\ccomp"  ?◎)
    ("\\all"    ?∀)
    ("\\ex"     ?∃)
    ("\\to"     ?→)
    ("\\sep"    ?∗)
    ("\\lc"     ?⌜)
    ("\\rc"     ?⌝)
    ("\\Lc"     ?⎡)
    ("\\Rc"     ?⎤)
    ("\\lam"    ?λ)
    ("\\empty"  ?∅)
    ("\\Lam"    ?Λ)
    ("\\Sig"    ?Σ)
    ("\\-"      ?∖)
    ("\\aa"     ?●)
    ("\\af"     ?◯)
    ("\\auth"   ?●)
    ("\\frag"   ?◯)
    ("\\iff"    ?↔)
    ("\\gname"  ?γ)
    ("\\incl"   ?≼)
    ("\\latert" ?▶)
    ("\\update" ?⇝)

    ;; accents (for iLöb)
    ("\\\"o" ?ö)

    ;; subscripts and superscripts
    ("^^+" ?⁺) ("__+" ?₊) ("^^-" ?⁻)
    ("__0" ?₀) ("__1" ?₁) ("__2" ?₂) ("__3" ?₃) ("__4" ?₄)
    ("__5" ?₅) ("__6" ?₆) ("__7" ?₇) ("__8" ?₈) ("__9" ?₉)

    ("__a" ?ₐ) ("__e" ?ₑ) ("__h" ?ₕ) ("__i" ?ᵢ) ("__k" ?ₖ)
    ("__l" ?ₗ) ("__m" ?ₘ) ("__n" ?ₙ) ("__o" ?ₒ) ("__p" ?ₚ)
    ("__r" ?ᵣ) ("__s" ?ₛ) ("__t" ?ₜ) ("__u" ?ᵤ) ("__v" ?ᵥ) ("__x" ?ₓ)

    ;; Greek alphabet
    ("\\Alpha"    ?Α) ("\\alpha"    ?α)
    ("\\Beta"     ?Β) ("\\beta"     ?β)
    ("\\Gamma"    ?Γ) ("\\gamma"    ?γ)
    ("\\Delta"    ?Δ) ("\\delta"    ?δ)
    ("\\Epsilon"  ?Ε) ("\\epsilon"  ?ε)
    ("\\Zeta"     ?Ζ) ("\\zeta"     ?ζ)
    ("\\Eta"      ?Η) ("\\eta"      ?η)
    ("\\Theta"    ?Θ) ("\\theta"    ?θ)
    ("\\Iota"     ?Ι) ("\\iota"     ?ι)
    ("\\Kappa"    ?Κ) ("\\kappa"    ?κ)
    ("\\Lamda"    ?Λ) ("\\lamda"    ?λ)
    ("\\Lambda"   ?Λ) ("\\lambda"   ?λ)
    ("\\Mu"       ?Μ) ("\\mu"       ?μ)
    ("\\Nu"       ?Ν) ("\\nu"       ?ν)
    ("\\Xi"       ?Ξ) ("\\xi"       ?ξ)
    ("\\Omicron"  ?Ο) ("\\omicron"  ?ο)
    ("\\Pi"       ?Π) ("\\pi"       ?π)
    ("\\Rho"      ?Ρ) ("\\rho"      ?ρ)
    ("\\Sigma"    ?Σ) ("\\sigma"    ?σ)
    ("\\Tau"      ?Τ) ("\\tau"      ?τ)
    ("\\Upsilon"  ?Υ) ("\\upsilon"  ?υ)
    ("\\Phi"      ?Φ) ("\\phi"      ?φ)
    ("\\Chi"      ?Χ) ("\\chi"      ?χ)
    ("\\Psi"      ?Ψ) ("\\psi"      ?ψ)
    ("\\Omega"    ?Ω) ("\\omega"    ?ω)

   )
  ;; use the newly-created math input method
  (set-input-method "math")
)

(add-hook! coq-mode
  (iris-input-config)
  ;; Map symbols to other for indentation
  (setq coq-smie-user-tokens
   '(("," . ":=")
     ("∗" . "->")
     ("-∗" . "->")
     ("∗-∗" . "->")
     ("==∗" . "->")
     ("=∗" . "->") 			;; Hack to match ={E1,E2}=∗
     ("|==>" . ":=")
     ("⊢" . "->")
     ("⊣⊢" . "->")
     ("↔" . "->")
     ("←" . "<-")
     ("→" . "->")
     ("=" . "->")
     ("==" . "->")
     ("/\\" . "->")
     ("⋅" . "->")
     (":>" . ":=")
     ("by" . "now")
     ("forall" . "now")              ;; NB: this breaks current ∀ indentation.
   ))

  ;; Avoid additional paren when adding comments
  ;;   https://github.com/tchajed/dotfiles/blob/master/emacs/doom/%2Bcoq.el
  (when (modulep! :config default +smartparens)
    (after! smartparens
      (sp-with-modes '(coq-mode)
        (sp-local-pair "(*" "*)")
        (sp-local-pair "(*" "*"
                     ;:actions '(insert)
                     :post-handlers '(("| " "SPC") ("|\n[i]*[d-2]" "RET")))
  )))


  ;; Overwriteing Proof Generals prettification list
  (setq-local prettify-symbols-alist
            '(("Qed." . ?🐣) ("Defined." . ?🐤) ("Admitted." . ?🍗)))
  (prettify-symbols-mode)

  ;; Based on Michael Sammler's comment.
  ;; https://mattermost.mpi-sws.org/iris/pl/8w7yujxjwfn9zg7usgj9ctwyhh
  ;; Relies on script opam-coqtop in PATH which calls coq through opam
  (setq coq-prog-name "opam-coqtop")
  ;; Disable electric indents
  (electric-indent-mode -1)
)
