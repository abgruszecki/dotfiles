;;; My functions

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

;;; org-mode

(defun my/org/set-ztk-id ()
  (interactive)
  (let* ((h (or (om-parse-this-headline) (error "Not looking at a headline!")))
         (s (om-get-property :raw-value h))
         (id (cadr (or (s-match "^#\\([^ ]+\\)" s)
                       (error "Headline doesn't have a ZTK id!")))))
    (org-set-property "CUSTOM_ID" id)))

(defun my/org/sort-todos ()
  (interactive)
  "Sort todo items, /my/ way."
  (unless (org-at-heading-p)
      (error "Not at a heading!"))
  (dolist (sort-type '(?p ?o)) (org-sort-entries nil sort-type))
  (org-cycle)
  (org-cycle)
  (org-unlogged-message "Sorted TODOs!"))

;;; projectile

(defun my/projectile/save-project-files (&optional ask)
  "Save files in current project."
  (interactive "P")
  (-let [project-root (projectile-project-root)]
    (save-some-buffers (not ask)
                       (lambda ()
                         (and buffer-file-name
                              (projectile-project-buffer-p (current-buffer)
                                                           project-root))))))

;;; perspective

(defvar my-perspective//loaded-all nil)
(defun my-perspective//load-all ()
  (unless my-perspective//loaded-all
    (persp-load-state-from-file "bespoke")
    (persp-load-state-from-file "para")
    (persp-load-state-from-file "dotty")
    (setq my-perspective//loaded-all t)))

(defun my-perspective/switch-to-bespoke ()
  (interactive)
  (unless (persp-with-name-exists-p "bespoke")
    (persp-load-state-from-file "bespoke"))
  (if (eq major-mode 'help-mode)
      (progn
        (pupo/close-window)
        (persp-switch "bespoke")
        (my/help-resume)))
  (persp-switch "bespoke"))

(defun my-perspective/switch-to-para ()
  (interactive)
  (unless (persp-with-name-exists-p "para")
    (persp-load-state-from-file "para"))
  (if (string= "para" (spacemacs//current-layout-name))
      (call-interactively #'org-roam-find-file)
    (persp-switch "para")))

(defun my-perspective/switch-to-dotty ()
  (interactive)
  (let ((p-name "dotty"))
    (unless (persp-with-name-exists-p p-name)
      (persp-load-state-from-file p-name))
    (persp-switch p-name)))

(defvar my-perspective//current-dynamic-bindings nil)
(defvar my-perspective/known-dynamic
  (list "scala3doc"
        "papers"
        "capture-calc"))

(defun my-perspective/switch-to-dynamic (force-pick &optional force-key)
  (interactive "P")
  (let* ((k (or force-key
                (elt (this-command-keys-vector) 0)))
         (entry (alist-get k my-perspective//current-dynamic-bindings)))
    (when (or force-pick (not entry))
      (-if-let (picked (helm :sources (helm-build-sync-source "Perspective name"
                                        :candidates my-perspective/known-dynamic)))
          (setf (alist-get k my-perspective//current-dynamic-bindings) picked
                entry picked)
        (user-error "No perspective picked."))
      )
    (unless (persp-with-name-exists-p entry)
      (persp-load-state-from-file entry))
    (persp-switch entry)))

(defun my/fixup-whitespace (&rest a)
  (when (or
         (and (eq (char-before) ?\{)
              (not (eq (char-after) ?\})))
         (and (eq (char-after) ?\})
              (not (eq (char-before) ?\{))))
    (insert ?\s)))
(advice-add #'evil-join :after #'my/fixup-whitespace)

;;; input method

(defvar my-greek/input-keymap (make-sparse-keymap))
(global-set-key (kbd "H-\\") my-greek/input-keymap)

(defun my-greek/krazy-self-insert ()
  "Krazy thing that looks up its own keybinding in the greek input keymap and inserts its own description."
  (interactive)
  (let* ((keys (this-command-keys-vector))
         (last-key (elt keys (1- (length keys))))
         (key (cadr (assoc last-key (cdr my-greek/input-keymap)))))
    (insert-char (elt key 0))))

(defmacro my-greek/define-key (key input)
  `(define-key my-greek/input-keymap (kbd ,key) (cons ,input #'my-greek/krazy-self-insert)))

;; Reminder: these depend on `which-key-enable-extended-define-key'
(define-key my-greek/input-keymap (kbd "H-a") (cons "∀" #'my-greek/krazy-self-insert))
(my-greek/define-key "a" "α")
(my-greek/define-key "b" "β")
(my-greek/define-key "d" "δ")
(my-greek/define-key "D" "Δ")
(my-greek/define-key "G" "Γ")
(my-greek/define-key "l" "λ")
(my-greek/define-key "L" "Λ")
(my-greek/define-key "|" "‣")
