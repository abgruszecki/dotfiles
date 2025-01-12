;;; $DOOMDIR/+workspaces.el -*- lexical-binding: t; -*-

(defun ~workspace/active-workspace-buffers ()
  "Taken from `+vertico--workspace-generate-sources'."
  (let ((w (+workspace-current-name)))
    (consult--buffer-query
     :sort 'visibility
     :as #'buffer-name
     :predicate (lambda (b)
                  (+workspace-contains-buffer-p b (+workspace-get w t))))
    ))

(defun ~workspace/switch-to-last-buffer ()
  (interactive)
  (switch-to-buffer (car (~workspace/active-workspace-buffers)))
  )

(map! :leader
      :n "C-," #'~workspace/switch-to-last-buffer)

(map! :leader :prefix "TAB"
 :n "b" #'~workspace/become
 )
