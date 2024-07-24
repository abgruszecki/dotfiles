;;; window-select.el -*- lexical-binding: t; -*-

(setq! winum-scope 'frame-local)

(map! :map global-map "C-<tab>" #'evil-switch-to-windows-last-buffer)
