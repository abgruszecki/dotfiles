;;; +exp/override-doom-leaders.el -*- lexical-binding: t; -*-

;; NOTE This did not work, because keymaps care about function cells,
;; and it's not possible to make the function cell of a symbol buffer-local.
;;
;; (defvar ~current-leader-keymap nil)
;; (defvar ~current-localleader-keymap nil)
;; (make-variable-buffer-local '~current-leader-keymap)
;; (make-variable-buffer-local '~current-localleader-keymap)
;; (map!
;;  "M-m" '~current-leader-keymap
;;  "M-," '~current-localleader-keymap)

;; NOTE It seems this would work, but I went for the built-in option instead,
;; otherwise describe-key is borked.
;; (keymap-global-set "M-m" 'doom/leader)
;; (add-hook!
;;  'after-change-major-mode-hook
;;  (defun ~set-current-leader-keymaps (&optional force)
;;    (unless (or force (keymap-local-lookup "M-m"))
;;      (keymap-local-set "M-m" (keymap-local-lookup "<normal-state> SPC"))
;;      )
;;    (unless (or force (keymap-local-lookup "M-,"))
;;      (keymap-local-set "M-," (keymap-local-lookup "<normal-state> SPC m"))
;;      )
;;    )
;;  )
