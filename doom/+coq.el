;; This is mostly copied from the coq module definition
;; Better not remove any bindings in order to keep all in one place
(map! :after coq-mode
      :map (coq-mode-map coq-goals-mode-map coq-response-mode-map)
      :localleader
      "n" #'proof-assert-next-command-interactive
      "u" #'proof-undo-last-successful-command
      "RET" #'proof-goto-point
      "." #'company-coq-jump-to-definition
      "x" #'proof-shell-exit
      (:prefix ("l" . "layout")
        "c" #'pg-response-clear-displays
        "l" #'proof-layout-windows
        "p" #'proof-prf)
      (:prefix ("p" . "proof")
        "i" #'proof-interrupt-process
        "p" #'proof-process-buffer
        "q" #'proof-shell-exit
        "r" #'proof-retract-buffer)
      (:prefix ("a" . "about/print/check")
        "p" #'coq-Print
        "P" #'coq-Print-with-all
        "b" #'coq-About
        "B" #'coq-About-with-all
        "c" #'coq-Check
        "C" #'coq-Check-show-all
        "c" #'coq-show
        "a" #'coq-Search
        "f" #'proof-find-theorems
        (:prefix ("i" . "implicits")
          "b" #'coq-About-with-implicits
          "c" #'coq-Check-show-implicits
          "i" #'coq-Print-with-implicits))
      (:prefix ("g" . "goto")
        "e" #'proof-goto-command-end
        "l" #'proof-goto-end-of-locked
        "s" #'proof-goto-command-start)
      (:prefix ("i" . "insert")
        "c" #'coq-insert-command
        "e" #'coq-end-Section
        "i" #'coq-insert-intros
        "r" #'coq-insert-requires
        "s" #'coq-insert-section-or-module
        "t" #'coq-insert-tactic
        "T" #'coq-insert-tactical))

;; set after package doesn't work :(
;; Configure when to change layout modes of proof general (1 column vs. 2 columns)
;; (setq split-width-threshold 190)

;; Change the major mode symbol for Coq (using delight)
(use-package! delight
  :config
  (delight 'coq-mode "🐓" :major)
  (delight 'coq-goals-mode "🐓 Goals" :major)
  (delight 'coq-response-mode "🐓 Response" :major)
)
;;)
;; Disable Company Coq Features (Including prettify-symbols)
(after! company-coq
  (add-to-list  'company-coq-disabled-features 'prettify-symbols))

;; ****************************************************
;; ********************* IRIS SYM *********************
;; ****************************************************

;; (defun iris-input-config ()
;;   "Set up math input for Iris.
;;     Based on https://gitlab.mpi-sws.org/iris/iris/-/blob/master/docs/editor.md, and
;;     https://github.com/tchajed/dotfiles/blob/master/emacs/doom/%2Bcoq.el"

;;   ;; Input method for the minibuffer
;;   (defun my-inherit-input-method ()
;;     "Inherit input method from `minibuffer-selected-window'."
;;     (let* ((win (minibuffer-selected-window))
;;            (buf (and win (window-buffer win))))
;;       (when buf
;;         (activate-input-method (buffer-local-value 'current-input-method buf)))))
;;   (add-hook 'minibuffer-setup-hook #'my-inherit-input-method)
;;   ;; Define the actual input method
;;   (quail-define-package "math" "UTF-8" "Ω" t)

;;   ;; ;; https://emacs.stackexchange.com/questions/76725/how-to-implement-a-function-in-quail-define-rules-for-set-input-method
;;   ;; (defun quail-action-f (key idx)    ; key=keyword, idx=length
;;   ;;   (quail-delete-region)                 ; these lines apparently needed
;;   ;;   (setq quail-current-str nil           ; to remove key.
;;   ;;         quail-converting nil            ; (not sure why all 4 is needed)
;;   ;;         quail-conversion-str "")        ;


;;   ;;   (insert "⟨⟩")
;;   ;;   (forward-char -1)

;;   ;;   (throw 'quail-tag nil)                ; this is need for finishing up?
;;   ;;   )

;;   (quail-define-rules ; add whatever extra rules you want to define here...
;;     ("\\fun"         ?λ)
;;     ("\\mult"        ?⋅)
;;     ("\\ent"         ?⊢)
;;     ("\\valid"       ?✓)
;;     ("\\diamond"     ?◇)
;;     ("\\box"         ?□)
;;     ("\\bbox"        ?■)
;;     ("\\later"       ?▷)
;;     ("\\pred"        ?φ)
;;     ("\\and"         ?∧)
;;     ("\\or"          ?∨)
;;     ("\\comp"        ?∘)
;;     ("\\ccomp"       ?◎)
;;     ("\\all"         ?∀)
;;     ("\\forall"      ?∀)
;;     ("\\ex"          ?∃)
;;     ("\\exists"      ?∃)
;;     ("\\to"          ?→)
;;     ("\\sep"         ?∗)
;;     ("\\lc"          ?⌜)
;;     ("\\rc"          ?⌝)
;;     ("\\Lc"          ?⎡)
;;     ("\\Rc"          ?⎤)
;;     ("\\lam"         ?λ)
;;     ("\\empty"       ?∅)
;;     ("\\Lam"         ?Λ)
;;     ("\\Sig"         ?Σ)
;;     ("\\-"           ?∖)
;;     ("\\aa"          ?●)
;;     ("\\af"          ?◯)
;;     ("\\auth"        ?●)
;;     ("\\frag"        ?◯)
;;     ("\\iff"         ?↔)
;;     ("\\gname"       ?γ)
;;     ("\\incl"        ?≼)
;;     ("\\latert"      ?▶)
;;     ("\\update"      ?⇝)
;;     ("\\lt"          ?⤳)
;;     ("\\elem"        ?∈)
;;     ("\\?"           ?¿)

;;     ;; accents (for iLöb)
;;     ("\\\"o" ?ö)

;;     ;; subscripts and superscripts
;;     ("^^+" ?⁺) ("__+" ?₊) ("^^-" ?⁻)
;;     ("__0" ?₀) ("__1" ?₁) ("__2" ?₂) ("__3" ?₃) ("__4" ?₄)
;;     ("__5" ?₅) ("__6" ?₆) ("__7" ?₇) ("__8" ?₈) ("__9" ?₉)

;;     ("__a" ?ₐ) ("__e" ?ₑ) ("__h" ?ₕ) ("__i" ?ᵢ) ("__k" ?ₖ)
;;     ("__l" ?ₗ) ("__m" ?ₘ) ("__n" ?ₙ) ("__o" ?ₒ) ("__p" ?ₚ)
;;     ("__r" ?ᵣ) ("__s" ?ₛ) ("__t" ?ₜ) ("__u" ?ᵤ) ("__v" ?ᵥ) ("__x" ?ₓ)

;;     ;; Greek alphabet
;;     ("\\Alpha"    ?Α) ("\\alpha"    ?α)
;;     ("\\Beta"     ?Β) ("\\beta"     ?β)
;;     ("\\Gamma"    ?Γ) ("\\gamma"    ?γ)
;;     ("\\Delta"    ?Δ) ("\\delta"    ?δ)
;;     ("\\Epsilon"  ?Ε) ("\\epsilon"  ?ε)
;;     ("\\Zeta"     ?Ζ) ("\\zeta"     ?ζ)
;;     ("\\Eta"      ?Η) ("\\eta"      ?η)
;;     ("\\Theta"    ?Θ) ("\\theta"    ?θ)
;;     ("\\Iota"     ?Ι) ("\\iota"     ?ι)
;;     ("\\Kappa"    ?Κ) ("\\kappa"    ?κ)
;;     ("\\Lamda"    ?Λ) ("\\lamda"    ?λ)
;;     ("\\Lambda"   ?Λ) ("\\lambda"   ?λ)
;;     ("\\Mu"       ?Μ) ("\\mu"       ?μ)
;;     ("\\Nu"       ?Ν) ("\\nu"       ?ν)
;;     ("\\Xi"       ?Ξ) ("\\xi"       ?ξ)
;;     ("\\Omicron"  ?Ο) ("\\omicron"  ?ο)
;;     ("\\Pi"       ?Π) ("\\pi"       ?π)
;;     ("\\Rho"      ?Ρ) ("\\rho"      ?ρ)
;;     ("\\Sigma"    ?Σ) ("\\sigma"    ?σ)
;;     ("\\Tau"      ?Τ) ("\\tau"      ?τ)
;;     ("\\Upsilon"  ?Υ) ("\\upsilon"  ?υ)
;;     ("\\Phi"      ?Φ) ("\\phi"      ?φ)
;;     ("\\Chi"      ?Χ) ("\\chi"      ?χ)
;;     ("\\Psi"      ?Ψ) ("\\psi"      ?ψ)
;;     ("\\Omega"    ?Ω) ("\\omega"    ?ω)

;;    )
;;   ;; use the newly-created math input method
;;   (set-input-method "math")
;; )
(add-hook! coq-mode
  ;;(iris-input-config)
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
  ;; DEPRECATED : See solution below
  ;; (setq coq-prog-name "opam-coqtop")

  ;; Skip proofs
  (setq proof-omit-proofs-option t)
  ;; Disable electric indents
  (electric-indent-mode -1)
)

;; Based on Michael Sammler's comment.
;; https://github.com/ProofGeneral/PG/issues/819
;; Relies on script opam-coqtop in PATH which calls coq through opam
(add-hook! 'proof-shell-before-process-hook
  (setq proof-prog-name "opam-coqtop")
  (setq coq-compiler (coq-detect-coqc))
  (setq coq-dependency-analyzer (coq-detect-coqdep))
  (setq coq-prog-name proof-prog-name)
  (setq coq-prog-args (coq-prog-args))
    ; 99 causes this hook to run very late, in particular after
    ; the hook from proof-general that sets these variables
    99)

;; Add column indicator
(add-hook! coq-mode #'display-fill-column-indicator-mode)
