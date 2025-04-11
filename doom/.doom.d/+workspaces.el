;;; $DOOMDIR/+workspaces.el -*- lexical-binding: t; -*-

;; Workspace navigation

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
 :n "b" #'~workspace/become)

(map! "M-1" nil
      "M-2" nil
      "M-3" nil
      "M-4" nil
      "M-5" nil
      "M-6" nil
      "M-7" nil
      "M-8" nil

      "C-<f1>" #'+workspace/switch-to-0
      "C-<f2>" #'+workspace/switch-to-1
      "C-<f3>" #'+workspace/switch-to-2
      "C-<f4>" #'+workspace/switch-to-3
      "C-<f5>" #'+workspace/switch-to-4
      "C-<f6>" #'+workspace/switch-to-5
      "C-<f7>" #'+workspace/switch-to-6
      "C-<f8>" #'+workspace/switch-to-7

      (:prefix "H-`"
               "<f1>" #'+workspace/switch-to-0
               "<f2>" #'+workspace/switch-to-1
               "<f3>" #'+workspace/switch-to-2
               "<f4>" #'+workspace/switch-to-3
               "<f5>" #'+workspace/switch-to-4
               "<f6>" #'+workspace/switch-to-5
               "<f7>" #'+workspace/switch-to-6
               "<f8>" #'+workspace/switch-to-7))
