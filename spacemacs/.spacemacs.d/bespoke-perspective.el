;; -*- mode: emacs-lisp; lexical-binding: t -*-

(setq persp-autokill-buffer-on-remove 'kill
      persp-kill-foreign-buffer-behaviour 'kill)

(add-to-list 'persp-filter-save-buffers-functions
              #'(lambda (b)
                  (with-current-buffer b
                    (derived-mode-p 'magit-mode))))

(defun bespoke/trace-persp-add (oldfun buf persp)
  (with-current-buffer (get-buffer-create "*persp-trace*")
    (insert (format "[%s] Adding %s\n" (persp-name persp) buf)))
  (funcall oldfun buf persp))

(advice-add 'persp--buffer-in-persps-add :around #'bespoke/trace-persp-add)

(defun bespoke/trace-kill-buffer (oldfun &optional buf)
  (setq buf (or buf (current-buffer)))
  (funcall oldfun buf)
  (when (buffer-live-p buf)
    (let* ((persps (--map (persp-name it) (persp--buffer-in-persps buf)))
            (msg (format "Undead buffer: %s (in persps: %s)" buf persps)))
      (message msg)
      (with-current-buffer (get-buffer-create "*persp-trace*")
        (insert msg "\n")))))

(advice-add #'kill-buffer :around #'bespoke/trace-kill-buffer)
