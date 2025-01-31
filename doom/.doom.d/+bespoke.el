;;; ../dotfiles/doom/.doom.d/+bespoke.el -*- lexical-binding: t; -*-

;;; Misc configuration which (currently?) doesn't fit anywhere else

(defun bsp/dwim-clear-screen (&optional arg)
  (interactive "P")
  (evil-ex-nohighlight)
  (unless (eq arg 0)
    (recenter-top-bottom arg)))

(defun ~yank-magit-current-commit-hash (prefix)
  (interactive "P")
  (let ((hash (if prefix
                  (magit-rev-parse "HEAD")
                (magit-rev-parse "--short" "HEAD")
                )))
    (kill-new hash)))

(defun ~yank-current-date (prefix)
  (interactive "P")
  (if prefix
    (kill-new (format-time-string "%d.%m.%Y"))
      (kill-new (format-time-string "%Y.%m.%d"))))

(defun ~new-scratch-templated ()
  (interactive)
  (if-let ((file (read-file-name "Select template:" "~/org/scratch/templates/")))
     (let ((buf (generate-new-buffer (concat "*scratch-" (f-base file) "*"))))
       (with-current-buffer buf
         (insert-file-contents file)
         (markdown-mode))
       (set-window-buffer nil buf))
    ))

;; TODO ...does this depend on being after the code which configures the name of the prefix?
(map! :leader
      "\\ Y G" #'~yank-magit-current-commit-hash
      "\\ Y D" #'~yank-current-date
      "\\ C-n" #'~new-scratch-templated)
