;;; $DOOMDIR/+window-select.el -*- lexical-binding: t; -*-

(setq! winum-scope 'frame-local)

(map! :map global-map "C-<tab>" #'evil-switch-to-windows-last-buffer)

(map! ; "C-<tab>" alternate centaur tab
      "C-`" #'~other-window ;; should this be other-buffer?
      "C-<escape>" #'+popup/toggle)

(map! :prefix "H-`"
      "<tab>" #'~other-window
      "H-<tab>" #'~other-window)

(map! "H-<tab>" #'~other-window)
