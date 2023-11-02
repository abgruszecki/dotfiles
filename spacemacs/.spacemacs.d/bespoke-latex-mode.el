(defun bsp/cdlatex-jump ()
  (interactive)
  "Jumps like `cdlatex-tab', without the other behaviour."
  (catch 'stop
    (while (re-search-forward "[ )}\n]\\|\\]" (point-max) t)
      (forward-char -1)
      (cond
       ((= (following-char) ?\ )
        ;; stop at first space or b-o-l
        (if (not (bolp)) (forward-char 1)) (throw 'stop t))
       ((= (following-char) ?\n)
        ;; stop at line end, but not after \\
        (if (and (bolp) (not (eobp)))
            (throw 'stop t)
          (if (equal "\\\\" (buffer-substring-no-properties
                             (- (point) 2) (point)))
              (forward-char 1)
            (throw 'stop t))))
       (t
        ;; Stop before )}] if preceding-char is any parenthesis
        (if (or (= (char-syntax (preceding-char)) ?\()
                (= (char-syntax (preceding-char)) ?\))
                (= (preceding-char) ?-))
            (throw 'stop t)
          (forward-char 1)
          (if (looking-at "[^_\\^({\\[]")
              ;; stop after closing bracket, unless ^_[{( follow
              (throw 'stop t))))))))

(evil-define-key 'motion LaTeX-mode-map
  (kbd "TAB") #'bsp/cdlatex-jump)

;; See https://www.gnu.org/software/emacs/manual/html_node/elisp/Face-Attributes.html
;; If I could use text properties, then adding an underline to prettified text seems good.
(defun bsp-latex//configure-prettify-symbols-mode ()
  (dolist (p `(("\\al" . ?α)
               ("\\be" . ?β)
               ("\\ga" . ?γ)
               ("\\de" . ?δ)
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
               ("\\SCtx" . ?Δ)

               ("\\Pos" . ?+)
               ("\\Neg" . ?-)

               ("\\Vstore" . ?σ)
               ("\\Otypstore" . 8764)
               ("\\Otypbind" . 8764)
               ;; Unfortunately this doesn't seem to work...
               ;; ("\\Neg" . ,(propertize "-" 'face '(:foreground "green")))
               ))
    (assoc-delete-all "--" prettify-symbols-alist)
    (assoc-delete-all "---" prettify-symbols-alist)
    (setf (alist-get (car p) prettify-symbols-alist nil nil #'equal)
          (cdr p))
    ;; (add-to-list 'prettify-symbols-alist p nil #'equal)
    )
  ;; TODO: set this only outside evil normal state? There are hooks for this.
  (setf prettify-symbols-unprettify-at-point 'right-edge)
  )

(defun bsp-latex//configure ()
  (make-local-variable 'comment-empty-lines)
  (setf comment-empty-lines t)
  )

(add-hook 'LaTeX-mode-hook #'prettify-symbols-mode)
(add-hook 'LaTeX-mode-hook #'bsp-latex//configure-prettify-symbols-mode)
(add-hook 'LateX-mode-hook #'bsp-latex//configure)

(defun bsp-latex//filter-tree (tree filter-fn)
  (->> tree
       (mapcan (lambda (vtx)
                 (if (listp vtx) (list (bsp-latex//filter-tree vtx filter-fn))
                   (if (apply filter-fn (list vtx)) (list vtx) nil)))))
  )

(defun bsp-latex//fix-company-backends ()
  (setf company-backends
        (bsp-latex//filter-tree
         company-backends
         ($ (not (eq $1 'company-auctex-symbols)))
         )))

(spacemacs|disable-company LaTeX-mode)

(defun bsp-latex//disable-company ()
  (company-mode 0))

(add-hook 'LaTeX-mode-hook #'bsp-latex//disable-company 90)
