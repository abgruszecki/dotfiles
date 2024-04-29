;; -*- mode: emacs-lisp; lexical-binding: t;  read-symbol-shorthands: (("my-p/" . "my-perspective/")) -*-

;;; My function

(require 'persist)

(defun my/make-lb ()
  (cons nil nil))

(defun my/lb-append (lb elt)
  (setf (cdr lb) (nconc (cdr lb) (list elt)))
  (unless (car lb)
    (setf (car lb) (cdr lb)))
  (setf (cdr lb) (last (cdr lb)))
  )

(defun my//org-roam--get-headlines (&optional file with-marker use-stack)
  "Return all outline headings for the current buffer.
If FILE, return outline headings for passed FILE instead.
If WITH-MARKER, return a cons cell of (headline . marker).
If USE-STACK, include the parent paths as well."
  (let* ((buf (or (and file
                       (or (find-buffer-visiting file)
                           (find-file-noselect file)))
                  (current-buffer)))
         (outline-level-fn outline-level)
         (path-separator "/")
         (stack-level 0)
         stack cands name level marker)
    (with-current-buffer buf
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward org-complex-heading-regexp nil t)
          (save-excursion
            (setq name (substring-no-properties (or (match-string 4) "")))
            (setq marker (point-marker))
            (when use-stack
              (goto-char (match-beginning 0))
              (setq level (funcall outline-level-fn))
              ;; Update stack.  The empty entry guards against incorrect
              ;; headline hierarchies, e.g. a level 3 headline
              ;; immediately following a level 1 entry.
              (while (<= level stack-level)
                (pop stack)
                (cl-decf stack-level))
              (while (> level stack-level)
                (push name stack)
                (cl-incf stack-level))
              (setq name (mapconcat #'identity
                                    (reverse stack)
                                    path-separator)))
            (push (if with-marker
                      (cons name marker)
                    name) cands)))))
    (nreverse cands)))

(defun my/foo ()
  (interactive)
  ;; (org-entry-properties)
  ;; (org-set-property "ZTK" "1")
  (helm :sources (helm-build-sync-source
                     "Headlines"
                   :candidates (my//org-roam--get-headlines nil t)
                   :action (lambda (m)
                             (let ((props (save-excursion
                                            (goto-char m)
                                            (org-entry-properties))))
                               (insert (format "[[id:%s][#%s]]"
                                               (cdr (assoc "ID" props))
                                               (cdr (assoc "ZTK" props))
                                               )))
                             )
                   ;; :persistent-action #'dotty-sbt/helm-setopt-toggle-option
                   )
        :buffer "*helm Dotty options*"))

(defun my-ztk/set-id (new-id)
  (interactive ;; "M"
               (list (read-string "ZTK ID: "
                                  ;; ""
                                  (condition-case nil
                                      (save-excursion
                                        (beginning-of-line)
                                        (org-up-element)
                                        (cdr (assoc "ZTK" (org-entry-properties))))
                                    (error ""))
                                  ))
               )
  (let* ((node (org-element-context))
         (new-id-str (format "#%s" new-id))
         (old-title (org-element-property :title node))
         (new-title (if (s-starts-with? "#" old-title)
                        (s-replace-regexp "^#[^ ]*" new-id-str old-title t t)
                      (concat new-id-str " " old-title)
                      )))
    (org-element-put-property node :title new-title)
    (kill-whole-line)
    (insert (org-element-interpret-data node))
    (forward-line -1)
    (org-ml-set-property :title (list new-title) node)
    (org-set-property "ZTK" new-id)
    (org-id-get-create)
    ;; (org-ml-headline-set-node-property "ZTK" new-id-str node)
    )

  )

(defun my-org/set-created ()
  (interactive)
  (save-excursion
    (while (not (org-at-heading-p))
      (org-up-element))
    (unless (org-entry-get (point) "CREATED")
      (org-set-property "CREATED" (format-time-string "[%Y-%m-%d]")))
    (org-set-property "UPDATED" (format-time-string "[%Y-%m-%d]")))
  )

(defun my/current-project-root ()
  (car (project-roots (project-current))))

(defun my/current-project-TODOs-file ()
  (concat (my/current-project-root) "/TODOs.org"))

(defun my/org-template/project-todo-capture ()
  (with-temp-buffer
    (insert-file-contents "~/.spacemacs.d/org-templates/project-todo.org")
    (buffer-string)))

(defun my/help-resume ()
  (interactive)
  (select-window (display-buffer "*Help*")))

;; TODO ask to save all files in project when trying to run a command in SBT?

(defun my/jump-to-indent (direction cmp)
  "Test doc"
  (interactive)
  (while (and (looking-at-p "^$")
              (zerop (forward-line (or direction 1)))))
  (let ((start-indent (current-indentation)))
    ;; TODO pick up the first indent along the way if the current line is empty
    (while (and (not (bobp))
                (zerop (forward-line (or direction 1)))
                (or (looking-at-p "^$")
                    ;; TODO ignore comment lines?
                    (cond
                     ((= 0 cmp) (> (current-indentation) start-indent))
                     ((> 0 cmp) (<= (current-indentation) start-indent))
                     ((< 0 cmp ) (>= (current-indentation) start-indent)))))))
  (back-to-indentation))

(defmacro my/def-indent-variant (name direction cmp)
  `(evil-define-motion ,name ()
     :type line
     :jump t
     (interactive)
     (my/jump-to-indent ,direction ,cmp)))

(my/def-indent-variant my/forwards-jump-to-outdent       1  1)
(my/def-indent-variant my/forwards-jump-to-same-indent   1  0)
(my/def-indent-variant my/forwards-jump-to-indent        1 -1)
(my/def-indent-variant my/backwards-jump-to-outdent     -1  1)
(my/def-indent-variant my/backwards-jump-to-same-indent -1  0)
(my/def-indent-variant my/backwards-jump-to-indent      -1 -1)

(defun my/jump-to-lesser-indent (direction)
  (interactive "P")
  (let ((start-indent (current-indentation)))
    (while
        (and (not (bobp))
             (zerop (forward-line (or direction 1)))
             (or (= (current-indentation) 0) ; should instead check if line is empty
                 (>= (current-indentation) start-indent)))))
  (back-to-indentation))

(defun my/grab-tmux ()
  "Grab dotty sbt output with tmux+ script"
  (interactive)
  (erase-buffer)
  (call-process "tmux+" nil (current-buffer) nil "output")
  (origami-reset (current-buffer))
  (origami-close-all-nodes (current-buffer)))

(evil-define-command my/slurp ()
  :repeat t
  ;; :suppress-operator t
  (interactive)
  (save-excursion
    (evil-up-paren ?{ ?} 1)
    (unless (or (move-text--at-last-line-p)
                (and (move-text--last-line-is-just-newline)
                     (move-text--at-penultimate-line-p)))
      (forward-line 1)
      (transpose-lines 1)
      (forward-line -2))
    (beginning-of-line)
    (unless (looking-at "\n")
      (indent-for-tab-command))))


;;; yasnippet

(defun my-yas/expand-dotty-println-it ()
  (interactive)
  (yas-expand-snippet (yas-lookup-snippet "dotty-debug-println-it")
                      (point-at-bol)
                      (point-at-eol)
                      `((yas-arg/it ,(buffer-substring (mark) (point))))))


;;; magit

(defun my/magit/kill-all-buffers ()
  (interactive)
  (mapc #'kill-buffer (magit-mode-get-buffers)))

(defun my-eyebrowse/toggle-magit ()
  (interactive)
  (if (eql -1 (eyebrowse--get 'current-slot))
      (eyebrowse-last-window-config)
    (let ((root (projectile-project-root))
          (init-needed? (not (eyebrowse--window-config-present-p -1))))
      (eyebrowse-switch-to-window-config -1)
      (when (and init-needed? root)
        (when (purpose-window-purpose-dedicated-p)
          (purpose-set-window-purpose-dedicated-p (selected-window) nil))
        (dired root)))))

(defun my-eyebrowse/reset-magit (root)
  (unless (eql -1 (eyebrowse--get 'current-slot))
    (user-error "incorrect workspace for this function!"))
  (dired root)
  (delete-other-windows))

(defun my-magit/display-buffer-function (buffer)
  (let ((root (projectile-project-root)))
    (when (eq 'magit-status-mode (buffer-local-value 'major-mode buffer))
      (when (not (eql -1 (eyebrowse--get 'current-slot)))
        (my-eyebrowse/toggle-magit))
      (my-eyebrowse/reset-magit root))
    (magit-display-buffer-traditional buffer)))

(defun my-magit/bury-buffer-function (&optional kill-buffer)
  (let ((restore-eyebrowse (eq 'magit-status-mode major-mode)))
    (quit-window kill-buffer (selected-window))
    (when restore-eyebrowse
      (eyebrowse-last-window-config))))

(setq magit-display-buffer-function #'my-magit/display-buffer-function
      magit-bury-buffer-function #'my-magit/bury-buffer-function)


;;; org-mode

(defun my-org/change-event-timestamps (&optional start?)
  (interactive "p")
  (unless (org-at-heading-p)
    (user-error "not at a heading!"))
  (unless (s-prefix? "past" (org-get-heading))
    (user-error "the heading isn't past"))
  (save-excursion
    (let ((pt (point))
          clk)
     (setf clk (progn
                 (org-clock-find-position t)
                 (org-element-at-point)))
     (unless (eq 'clock (car clk))
       ;; todo should we assume the journal is malformed? and fix it? maybe have an auxiliary function for that?
       (user-error "no clock entry!"))
     (unless (eq 'closed (org-element-property :status clk))
       (user-error "the clock is running!"))
     (save-excursion
       (forward-line)
       (unless (eq 'drawer (car (org-element-at-point)))
         (user-error "there are more than one clock entries!")))

     (goto-char (+ (if start? 0 -1) (org-element-property (if start? :begin :end) (org-element-property :value clk))))
     (org-time-stamp-inactive)          ; modify the time stamp interactively
     (org-clock-update-time-maybe)

     (setf clk (org-element-at-point))
     (let* ((duration (org-element-property :raw-value (org-element-property :value clk)))
            (stamp (if start?
                       (substring duration 1 (string-match (rx "]--") duration))
                     (substring duration (+ 3 (string-match (rx "--[") duration)) -1))))
       (goto-char pt)
       (if start?
           (org-add-planning-info 'scheduled stamp)
         (org-add-planning-info 'closed stamp)
         )
       )
     ))
)

(defun my/org/set-ztk-id ()
  (interactive)
  (let* ((h (or (om-parse-this-headline) (error "not looking at a headline!")))
         (s (om-get-property :raw-value h))
         (id (cadr (or (s-match "^#\\([^ ]+\\)" s)
                       (error "headline doesn't have a ztk id!")))))
    (org-set-property "custom_id" id)))

(defun my/org/sort-todos ()
  (interactive)
  "sort todo items, /my/ way."
  (unless (org-at-heading-p)
      (error "not at a heading!"))
  (dolist (sort-type '(?p ?o)) (org-sort-entries nil sort-type))
  (org-cycle)
  (org-cycle)
  (org-unlogged-message "sorted todos!"))

(defun my-org//jump-to-headline (headline)
  (re-search-forward
   (format org-complex-heading-regexp-format
           (regexp-quote headline))
   nil
   t))

(defun my-org/jump-to-events ()
  (interactive)
  (my-perspective/switch-to-para)
  (find-file "~/org/roam/captured.org")
  (my-org//jump-to-headline "wydarzenia")
  (outline-hide-sublevels 1)
  (outline-show-children)
  )

(defun my-org/sort-todos ()
  (interactive)
  (unless (my-org//up-top-heading)
    (user-error "not under a top-level heading!"))
  (org-sort-entries nil ?f #'org-element-at-point #'my-org//compare-todo-headings)
  (outline-hide-subtree)
  (outline-show-children))

(defun my-org//trace-compare-todo-headings (self fst snd)
  (let ((res (funcall self fst snd)))
    (message "comparing %s / %s : %s"
             (org-element-property :title fst)
             (org-element-property :title snd)
             res
             )
    res
    ))

;; (advice-add 'my-org//compare-todo-headings :around #'my-org//trace-compare-todo-headings)
;; (advice-remove 'my-org//compare-todo-headings #'my-org//trace-compare-todo-headings)

(defun my-org//compare-todo-headings (fst snd)
  (cl-macrolet ((do-compare (op accessor)
                            `(,op (,@accessor fst)
                                  (,@accessor snd)))
                ;; compare begin positions of trees to avoid sort-subr mangling heading order
                (do-compare-pos ()
                                `(do-compare <= (org-element-property :begin))))
    (cl-labels ((todo-kw-ord (elt)
                             (cl-position (org-element-property :todo-keyword elt)
                                          org-todo-keywords-1
                                          :test #'string=))
                (todo-prio-ord (elt)
                               (or (org-element-property :priority elt) org-priority-default))
                (closed-ord (elt)
                            (org-ml-time-to-unixtime
                             (org-ml-timestamp-get-start-time
                              (org-element-property :closed elt)
                              ))
                            ))
      (cond
       ((org-element-property :archivedp snd) t)
       ((org-element-property :archivedp fst) nil)
       ((not (org-element-property :todo-keyword fst)) (do-compare-pos))
       ((not (org-element-property :todo-keyword snd)) nil)
       (t
        (let ((fst-todo-type (org-element-property :todo-type fst))
              (snd-todo-type (org-element-property :todo-type snd))
              (todo-kw-cnt (length org-todo-keywords-1)))
          (cond
           ((not (eq fst-todo-type snd-todo-type))
            (eq fst-todo-type 'todo))
           ((eq fst-todo-type 'todo)
            (let ((fst-idx (todo-kw-ord fst))
                  (snd-idx (todo-kw-ord snd)))
              (if (= fst-idx snd-idx)
                  (if (= (todo-prio-ord fst)
                         (todo-prio-ord snd))
                      (do-compare-pos)
                    (<= (todo-prio-ord fst)
                        (todo-prio-ord snd)))
                (< fst-idx snd-idx))))
           ((eq fst-todo-type 'done)
            (if (= (closed-ord fst)
                   (closed-ord snd))
                (do-compare-pos)
              (>= (closed-ord fst)
                  (closed-ord snd))))
           (t (error "???")))))))))

(defun my-org//up-top-heading ()
  (or (and (org-at-heading-p) (eql (funcall outline-level) 1))
      (re-search-backward (rx bol "* ") nil t)))

(defun my-org/set-code ()
  (interactive)
  (save-excursion
    (back-to-indentation)
    (unless (looking-at (rx "#+begin_src"))
      (user-error "not at a code block!"))
    (goto-char (match-end 0))
    (delete-region (point) (line-end-position))
    (let ((picked (helm :sources (list (helm-build-sync-source
                                           "well-known"
                                           :candidates (list "scala" "coq"))
                                       (helm-build-dummy-source
                                           "bespoke")))))
      (insert " " picked))))

(defun my-org//list-code-block-strings ()
  (let ((lb (my/make-lb)))
    (save-excursion
      (goto-char (point-min))
      (while (let ((case-fold-search t))
               (re-search-forward (rx bol (* " ") "#+begin_src") nil t))
        (skip-chars-forward " ")
        (my/lb-append lb (substring-no-properties (buffer-substring (point) (point-at-eol))))))
    (cl-delete-duplicates (car lb) :test #'string=)))

(defun my-org/insert-code-block ()
  (interactive)
  (org-insert-structure-template "src")
  (let ((picked (helm :sources (list (helm-build-sync-source
                                         "well-known"
                                       :candidates (list "scala"
                                                         "coq"
                                                         "elisp"
                                                         "bash"
                                                         "haskell"
                                                         "python"
                                                         "text"))
                                     (helm-build-sync-source
                                         "this buffer"
                                       :candidates (my-org//list-code-block-strings))
                                     (helm-build-dummy-source
                                         "bespoke")))))
    (insert picked)))

(defun my-org/test ()
  (interactive)
  (let ((fs (org-agenda-files)))
    ;; allows checking which file is which
    (concat
     "category={"
     (s-join "\\|"
             (->> (org-roam-db-query [:select * :from tags :where (in file $v1)]
                                     (apply #'vector fs))
               (seq-filter (lambda (el)
                             (seq-contains-p (cadr el) "@zasób")))
               (seq-map (lambda (el)
                          (concat "\\(^" (my-org/test2 (car el)) "$\\)")))))
     "}")))

(defun my-org-roam/jump-to-agenda-file ()
  (interactive)
  (let* ((fs (org-agenda-files))
         (q (org-roam-db-query [:select * :from titles :where (in file $v1)]
                               (apply #'vector fs)))
         (titled (seq-map (lambda (el) (cons (cadr el) (car el))) q))
         (picked (helm :sources (list (helm-build-sync-source
                                          "agenda files"
                                        :candidates titled)))))
    (my-perspective/switch-to-para)
    (find-file picked)))

;; (setq attempt2 (caar (my-org/test)))

(defun my-org/test2 (file)
  (interactive)
  (with-current-buffer (find-file-noselect file)
    (let ((kw (org-collect-keywords '("category"))))
      (and kw (cadar kw)))))

(defun bespoke-org/capture-finalize-and-jump ()
  (interactive)
  (org-capture-put :jump-to-captured t)
  (org-capture-finalize))


;;; org-super-agenda

(defun my-super-agenda/go ()
  (interactive)
  (let* ((work-categories (list "dotty" "doctool" "students" "ta"))
         (org-super-agenda-groups
          `((:name "scheduled"
                   (:scheduled past
                                    :scheduled today))
            (:name "work (todo)"
                   :and (:category ,work-categories :todo ("todo")))
            (:name "work (task)"
                   :and (:category ,work-categories :todo ("task")))
            (:name "work (problems)"
                   :and (:category ,work-categories :todo ("open")))
            (:name "empty"
                   :discard (:anything t))
            )))
    (org-agenda nil "t")))

(defun my-super-agenda/bespoke-main (&optional nokeys)
  (interactive)
  (let* (; (attempt (my-org/test))
         )
    (org-agenda nil (if nokeys nil "t")))
  )


;;; projectile

(defun my-projectile/save-project-files (&optional ask)
  "save files in current project."
  (interactive "p")
  (-let [project-root (projectile-project-root)]
    (save-some-buffers (not ask)
                       (lambda ()
                         (and buffer-file-name
                              (projectile-project-buffer-p (current-buffer)
                                                           project-root))))))

;; This seems unnecessary?!?
;; (defun my/fixup-whitespace (&rest a)
;;   (when (or
;;          (and (eq (char-before) ?\{)
;;               (not (eq (char-after) ?\})))
;;          (and (eq (char-after) ?\})
;;               (not (eq (char-before) ?\{))))
;;     (insert ?\s)))
;; (advice-add #'evil-join :after #'my/fixup-whitespace)


;;; my-greek

(defvar my-greek/input-keymap (make-sparse-keymap))
(global-set-key (kbd "H-\\") my-greek/input-keymap)
(define-key my-greek/input-keymap (kbd "H-\\") #'insert-char)

(defun my-greek/krazy-self-insert ()
  "krazy thing that looks up its own keybinding in the greek input keymap and inserts its own description."
  (interactive)
  (let* ((keys (this-command-keys-vector))
         (last-key (elt keys (1- (length keys))))
         (key (cadr (assoc last-key (cdr my-greek/input-keymap)))))
    (insert-char (elt key 0))))

(defmacro my-greek/define-key (key input)
  `(define-key my-greek/input-keymap (kbd ,key) (cons ,input #'my-greek/krazy-self-insert)))

;; reminder: these depend on `which-key-enable-extended-define-key'
(define-key my-greek/input-keymap (kbd "h-a") (cons "∀" #'my-greek/krazy-self-insert))
(my-greek/define-key "a" "α")
(my-greek/define-key "b" "β")
(my-greek/define-key "d" "δ")
(my-greek/define-key "d" "δ")
(my-greek/define-key "g" "γ")
(my-greek/define-key "l" "λ")
(my-greek/define-key "l" "λ")
(my-greek/define-key "|" "‣")
(my-greek/define-key "1" "⩒")
(my-greek/define-key "2" "≗")

(defun bespoke/lambda-input-method ()
  "lambda is go!"
  (quail-define-package "lambda" "utf-8" "λ"
                        t ; guidance
                        nil nil nil
                        nil ; deterministic
                        nil nil
                        t ; maximum-shortest
                        )
  ;; This is a hack to allow multiple translation variants without taking over digit keys.
  ;; `quail-define-package' for whatever reason bundles together DETERMINISTIC and FORGET-LAST,
  ;; but if only DETERMINISTIC is set, the package does exactly what I want.
  (setf (nth 7 (quail-package "lambda")) ; deterministic
        t
        )
  (quail-define-rules
   (";;{" "⸢")
   (";;}" "⸣")
   (";;[" "⸤")
   (";;]" "⸥")
   (";[]" "□")
   ;; (";[" "[")
   ;; (";]" "]")
   (";0" "₀")
   (";1" "₁")
   (";2" "₂")
   (";3" "₃")
   (";4" "₄")
   (";5" "₅")
   (";6" "₆")
   (";7" "₇")
   (";8" "₈")
   (";9" "₉")
   (";a" "α")
   (";b" "β")
   (";d" "δ")
   (";e" "ε")
   (";f" "ϕ")
   (";l" "λ")
   (";g" "γ")
   (";g" "Γ")
   (";h" "θ")
   (";m" "μ")
   (";n" "ν")
   (";p" "ψ")
   (";r" "ρ")
   (";s" "σ")
   (";t" "τ")
   (";z" "ϕ")
   (";D" "Δ")
   (";G" "Γ")
   (";L" "Λ")
   (";P" "Ψ")
   (";S" "Σ")
   (";X" "Ξ")
   (";tp" "⊤")
   (";bt" "⊥")
   (";=" "≜")
   (";/=" "≠")
   (";-" "⊢")
   (";->" "→")
   (";<" "≤")
   (";>" "≥")
   (";." "·")
   (";ts" "⊢")
   (";to" "↦")
   (";or" "∨")
   (";and" "∧")
   (";all" "∀")
   (";fa" "∀")
   (";ex" "∃")
   (";box" "□")
   (";meet" "⊓")
   (";join" "⊔")
   (";sc" "⊂")
   (";su" "∪")
   (";si" "∩")
   (";>>" "»")
   (";<<" "«")
   (";oo" "∘")
   (";oO" "●")
   ))

(bespoke/lambda-mode)


;;; bespoke tab-line-mode

(defun bsp-tab-line-mode ()
  "Toggle `tab-line-mode' in current buffer and all others sharing a major mode with it."
  (interactive)
  (let* ((current-mode major-mode)
         (sym-current-mode-hooks (intern (concat (symbol-name current-mode) "-hook"))))
    (if (-contains? (symbol-value sym-current-mode-hooks) #'tab-line-mode)
        (remove-hook sym-current-mode-hooks #'tab-line-mode)
      (add-hook sym-current-mode-hooks #'tab-line-mode))
    (-each (buffer-list) ($ (with-current-buffer $1
                              (when (eq major-mode current-mode)
                                (tab-line-mode 'toggle)))))
    )
  )
