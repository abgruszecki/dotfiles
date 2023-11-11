;;; +tex.el -*- lexical-binding: t; -*-

(setq! +latex-indent-item-continuation-offset 'auto
       TeX-electric-sub-and-superscript nil
       )

;; Slowly figuring these out by reading https://www.gnu.org/software/auctex/manual/auctex/Fontification-of-macros.html.
;; TODO I should play with `font-latex-user-keyword-classes'.
;; TODO Does this work? See `font-latex-add-keywords'.
(setq! font-latex-match-italic-command-keywords '(("Xem" "{")))

;; HACK Overrides the base definition to take evil state into account.
(defun prettify-symbols--post-command-hook ()
  (cl-labels ((get-prop-as-list
               (prop)
               (remove nil
                       (list (get-text-property (point) prop)
                             (when (and (eq prettify-symbols-unprettify-at-point 'right-edge)
                                        (not (bobp)))
                               (get-text-property (1- (point)) prop))))))
    ;; Re-apply prettification to the previous symbol.
    (when (and prettify-symbols--current-symbol-bounds
               (or (< (point) (car prettify-symbols--current-symbol-bounds))
                   (> (point) (cadr prettify-symbols--current-symbol-bounds))
                   (and (not (eq prettify-symbols-unprettify-at-point 'right-edge))
                        (= (point) (cadr prettify-symbols--current-symbol-bounds)))))
      (apply #'font-lock-flush prettify-symbols--current-symbol-bounds)
      (setq prettify-symbols--current-symbol-bounds nil))
    ;; Unprettify the current symbol.
    (unless (memq evil-state '(normal operator))
      (when-let* ((c (get-prop-as-list 'composition))
                  (s (get-prop-as-list 'prettify-symbols-start))
                  (e (get-prop-as-list 'prettify-symbols-end))
                  (s (apply #'min s))
                  (e (apply #'max e)))
        (with-silent-modifications
          (setq prettify-symbols--current-symbol-bounds (list s e))
          (remove-text-properties s e '(composition nil)))))))

(defun ~tex/configure-prettify-symbols-mode ()
  (dolist (p `(("\\al" . ?α)
               ("\\be" . ?β)
               ("\\ga" . ?γ)
               ("\\de" . ?δ)
               ("\\ep" . ?ϵ)
               ("\\lam" . ?λ)
               ("\\tha" . ?θ)
               ("\\ty" . ?τ)
               ("\\tz" . ?σ)

               ("\\Ga" . ?Γ)
               ("\\De" . ?Δ)

               ("\\ts" . ?⊢)
               ("|->" . ?↦)
               ("|-" . ?⊢)
               ("=<" . ?≤)
               ("->" . ?→)

               ("\\fa" . ?∀)
               ("\\ex" . ?∃)

               ("\\CCtx" . ?Ξ)
               ("\\CDtx" . ?Σ)
               ("\\VCtx" . ?V)
               ("\\VDtx" . ?W)
               ("\\VEtx" . ?Q)
               ("\\SCtx" . ?Δ)

               ("\\Pos" . ?+)
               ("\\Neg" . ?-)

               ("\\ctxConcat" . ?⋅)
               ("\\ctxConcatNs" . ?⋅)
               ("\\ctxPush" . ?⋅)
               ("\\ctxPushNs" . ?⋅)

               ("\\Vstore" . ?σ)
               ("\\Otypstore" . ?∼)
               ("\\Otypbind" . ?∼)
               ;; Unfortunately this doesn't seem to work...
               ;; ("\\Neg" . ,(propertize "-" 'face '(:foreground "green")))
               ))
    (assoc-delete-all "--" prettify-symbols-alist)
    (assoc-delete-all "---" prettify-symbols-alist)
    (setf (alist-get (car p) prettify-symbols-alist nil nil #'equal)
          (cdr p))
    )
  (setf prettify-symbols-unprettify-at-point 'right-edge)
)

(defun ~tex/configure ()
  (~tex/configure-prettify-symbols-mode)

  (make-local-variable 'comment-empty-lines)
  (setf comment-empty-lines t
        electric-indent-inhibit t)

  (buffer-face-set '(:family "Julia Mono" :height 0.85))

  (prettify-symbols-mode 1)

  (when (fboundp #'flycheck-mode)
    (flycheck-mode 0))
  )

;; TODO this can be removed... right?
(defun ~tex//ensure-evil-tex ()
  (evil-tex-mode)
  (message "evil-tex-mode: %s" evil-tex-mode)
  )

;; (defun ~wip-evil-iedit-latex-env ()
;;   (interactive)
;;   (evil-iedit-state/iedit-mode 1)
;;   (save-excursion
;;     (if (s-contains-p "\\begin" (thing-at-point 'line))
;;         (iedit-expand-down-to-occurrence)
;;       (iedit-expand-up-to-occurrence))))

(after! tex
  (evil-declare-motion #'LaTeX-find-matching-begin)
  (evil-declare-motion #'LaTeX-find-matching-end)

  (dolist (fn '(evil-tex-go-back-section
                evil-tex-go-forward-section
                LaTeX-find-matching-begin
                LaTeX-find-matching-end
                ))
    (evil-set-command-properties fn :jump t))

  (add-hook! '(LaTeX-mode-hook)
             #'hl-todo-mode
             #'~tex//ensure-evil-tex
             )
  )

(evil-define-text-object ~evil-inner-dollar (count &optional beg end type)
  (evil-select-paren (rx ?$) (rx ?$) beg end type count nil))

(evil-define-text-object ~evil-a-dollar (count &optional beg end type)
  (evil-select-paren (rx ?$) (rx ?$) beg end type count t))

(map!
 :after tex
 :mode LaTeX-mode
 :textobj "$" #'~evil-inner-dollar #'~evil-a-dollar
 :m "[ M-[" #'LaTeX-find-matching-begin
 :m "] M-]" #'LaTeX-find-matching-end
 )
