;;; ../dotfiles/doom/.doom.d/+bespoke.el -*- lexical-binding: t; -*-

;;; Misc configuration which (currently?) doesn't fit anywhere else

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

;; TODO ...does this depend on being after the code which configures the name of the prefix?
(map! :leader
      "\\ Y G" #'~yank-magit-current-commit-hash
      "\\ Y D" #'~yank-current-date)
