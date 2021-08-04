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
