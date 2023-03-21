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

(general-define-key
 :keymaps 'LaTeX-mode-map
 "TAB" #'bsp/cdlatex-jump)

(defun bsp-latex//set-prettify-symbols-alist ()
  (dolist (p `(("\\al" . ?α)
               ("\\be" . ?β)
               ("\\ga" . ?γ)
               ("\\de" . ?δ)
               ("\\lam" . ?λ)
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
               ))
    (add-to-list 'prettify-symbols-alist p nil #'equal)
    (assoc-delete-all "--" prettify-symbols-alist)
    )
  )

(add-hook 'LaTeX-mode-hook #'prettify-symbols-mode)
(add-hook 'LaTeX-mode-hook #'bsp-latex//set-prettify-symbols-alist)

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
