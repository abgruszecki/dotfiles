;;; My functions

(defun my/current-project-root ()
  (car (project-roots (project-current))))

(defun my/current-project-TODOs-file ()
  (concat (my/current-project-root) "/TODOs.org"))

(defun my/org-template/project-todo-capture ()
  (with-temp-buffer
    (insert-file-contents "~/.spacemacs.d/org-templates/project-todo.org")
    (buffer-string)))

(defun my/show-last-help ()
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
  (my-perspective//load-all)
  (spacemacs/persp-switch-to-2))

(defun my-perspective/switch-to-para ()
  (interactive)
  (my-perspective//load-all)
  (spacemacs/persp-switch-to-3))

(defun my-perspective/switch-to-dotty ()
  (interactive)
  (my-perspective//load-all)
  (spacemacs/persp-switch-to-4))

(defun my/fixup-whitespace (&rest a)
  (when (or
         (and (eq (char-before) ?\{)
              (not (eq (char-after) ?\})))
         (and (eq (char-after) ?\})
              (not (eq (char-before) ?\{))))
    (insert ?\s)))
(advice-add #'evil-join :after #'my/fixup-whitespace)

;;; input method

(defvar my/greek-input-keymap (make-keymap))
(global-set-key (kbd "H-\\") my/greek-input-keymap)

(defmacro my/define-input-key (prefix key input &rest rest)
  `(progn
     (define-key my/greek-input-keymap ,key (lambda () (interactive) (insert-char ,(elt input 0))))
     (which-key-add-key-based-replacements ,(concat prefix " " key) ,input)
     ,@(if rest (cdr (macroexpand-all `(my/define-input-key ,prefix ,@rest))))
     ))

(my/define-input-key "H-\\"
                     "a" "α"
                     "b" "β"
                     "d" "δ"
                     "D" "Δ"
                     "G" "Γ"
                     "l" "λ"
                     "L" "Λ"
                     "|" "‣")
