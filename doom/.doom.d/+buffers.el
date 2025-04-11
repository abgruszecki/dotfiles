;;; $DOOMDIR/+workspaces.el -*- lexical-binding: t; -*-

;;; Buffer navigation

;; Also see https://www.emacswiki.org/emacs/iflipb.
(defun ~flip-last-buffer ()
  "Flip to the last active buffer.

Should act the same as SPC , RET."
  (interactive)
  ;; Taken from the implementation of `+vertico/switch-workspace-buffer',
  ;; concretely from `+vertico--workspace-generate-sources'.

  (let* ((active-workspace (+workspace-get (+workspace-current-name)))
         (buffers (consult--buffer-query
                   :sort 'visibility
                   ;;:as #'buffer-name
                   :predicate
                   (lambda (buf)
                     (+workspace-contains-buffer-p buf active-workspace)))))
    (funcall consult--buffer-display (car buffers))
    )
  )

(defun ~other-window (count &optional all-frames interactive)
  (interactive "p\ni\np")
  (other-window count all-frames interactive))

(map! :nvi "C-<tab>" #'~flip-last-buffer)
