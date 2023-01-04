;; -*- mode: emacs-lisp; lexical-binding: t -*-

(setq yequake-frames
        '(("Scratch" . ((buffer-fns "*scratch*")
                        (width . 1.0)
                        (height . 1.0)
                        (left . 0)
                        (top . 0)
                        (frame-parameters
                         (skip-taskbar . t)
                         (sticky . t)
                         (undecorated . t))))
          ("org-capture" . ((buffer-fns my-perspective/switch-to-para
                                        bespoke-yequake/org-capture)
                            (width . 1.0)
                            (height . 1.0)
                            (left . 0)
                            (top . 0)
                            (frame-parameters
                             (skip-taskbar . t)
                             (sticky . t)
                             (undecorated . t)))
           )
          ("org-agenda" . ((buffer-fns my-perspective/switch-to-para
                                       bespoke-yequake/org-agenda)
                           (width . 1.0)
                           (height . 1.0)
                           (left . 0)
                           (top . 0)
                           (frame-parameters
                            (skip-taskbar . t)
                            (sticky . t)
                            (undecorated . t))
                           ))))

(defun bespoke-yequake/org-capture (&optional goto keys)
  "Copied from `yequake-org-capture'.
Call `org-capture' in a Yequake frame.
Adds a function to `org-capture-after-finalize-hook' that closes
the recently toggled Yequake frame and removes itself from the
hook.

Note: if another Yequake frame is toggled before the capture is
finalized, when the capture is finalized, the wrong Yequake frame
will be toggled."
  (let* ((remove-hook-fn (lambda ()
                           (remove-hook 'org-capture-after-finalize-hook #'yequake-retoggle))))
    ;; (add-hook 'org-capture-after-finalize-hook remove-hook-fn)
    ;; (add-hook 'org-capture-after-finalize-hook #'yequake-retoggle)
    ;; MAYBE: Propose an `org-capture-switch-buffer-fn' variable that could be rebound here.

    ;; NOTE: We override `org-switch-to-buffer-other-window' because
    ;; it always uses `switch-to-buffer-other-window', and we want to
    ;; display the template menu and capture buffer in the existing
    ;; window rather than splitting the frame.
    (cl-letf* (((symbol-function #'org-switch-to-buffer-other-window)
                (symbol-function #'switch-to-buffer)))
      (condition-case nil
          (progn
            (org-capture goto keys)
            ;; Be sure to return the "CAPTURE-" buffer, which is the current
            ;; buffer at this point.
            (current-buffer))
        ((error quit)
         ;; Capture aborted: remove the hook and hide the capture frame.
         ;; (remove-hook 'org-capture-after-finalize-hook #'yequake-retoggle)
         (yequake-retoggle))))))

(defun bespoke-yequake/org-agenda ()
  (my-super-agenda/bespoke-main)
  (spacemacs/toggle-maximize-buffer)
  (current-buffer))
