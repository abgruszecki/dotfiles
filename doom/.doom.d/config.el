;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

(setq doom-leader-alt-key "M-m"
      doom-localleader-key ","
      doom-localleader-alt-key "M-,")

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Aleksander Boruch-Gruszecki"
      user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))
;; (setq doom-variable-pitch-font doom-font)
(setq doom-font (font-spec :family "Fantasque Sans Mono"
                           :size 13.0
                           ;; :spacing ?d
                           :weight 'normal
                           :width 'normal)
      ;; doom-symbol-font "Linux Libertine Display O"
      doom-symbol-font "Julia Mono"
      )

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;;; Emacs config
(setf (alist-get 'undecorated default-frame-alist) t)

(setq! compilation-skip-threshold 2
       compilation-scroll-output 'first-error)

(setq! pop-up-windows nil)

(setq! frame-title-format `("[" (:eval (safe-persp-name (get-current-persp))) "] %b – Doom Emacs"))

(add-to-list 'default-frame-alist '(fullscreen . maximized))

;;; Doom module config
(setq! +evil-want-o/O-to-continue-comments nil)

(map! "M-=" #'universal-argument
      (:map universal-argument-map "M-=" #'universal-argument-more)
      )

;; TODO I forgot to load a file yet another time. That should be automatic.
(defmacro load-config-fragments (&rest names)
  (cons 'progn
        (mapcar
         (lambda (name)
           `(condition-case err
                (load! ,name)
              (error
               (message "Error when loading %s: %s" ,name err))))
         names
         ))
  )

(load-config-fragments
 "+org"
 ;;
 "+window-select"
 "+workspaces"
 "+buffers"
 ;;
 "+lambda-input-method"
 ;;
 "+embark"
 "+tex"
 "+ai"
 ;;
 "+bespoke"
 )
;; (load! "+org")
;; (load! "+window-select")
;; ;;
;; (load! "+workspaces")
;; (load! "+lambda-input-method")
;; ;;
;; (load! "+embark")
;; (load! "+tex")
;; (load! "+ai")
;; ;;
;; (load! "+bespoke")

;;; Extra configs
;; Install grammars using M-x treesit-install-language-grammar
(pushnew! treesit-extra-load-path "~/opt/tree-sitter-grammars")

;;;; tweak citar for markdown-mode
(after! (citar markdown-mode)
  (map! :map markdown-mode-map
        :localleader
        :n "@" #'citar-insert-citation)
  )

(after! citar-markdown
  ;; NOTE `citar-markdown' is loaded only when citar needs markdown-specific code.
  (setq! citar-markdown-prompt-for-extra-arguments nil))

;;;; tame evil-snipe
;; TODO This had to be eval'd manually. Should it only be run after loading evil-snipe?
;; (add-hook! doom-first-input-hook ...)
;; (after! evil-snipe ...)
(map! (:map evil-snipe-local-mode-map
       :nv "s" nil
       :nv "S" nil
       :nvo "C-s" #'evil-snipe-s
       :nvo "C-S-s" #'evil-snipe-S)
      (:map evil-surround-mode-map
       :v "s" nil
       :v "S" nil)
      ;; these commands load evil-surround I think
      :v "s" #'evil-surround-region
      :v "S" #'evil-Surround-region
      :n "s" #'evil-substitute
      :n "S" #'evil-change-whole-line
      )

;; (if (not (eq (car company-global-modes) 'not))
;;     (warn "`company-global-mode' is not as expected!")
;;   (setq! company-global-modes
;;          (nconc `(not org-mode LaTeX-mode)
;;                 (cdr company-global-modes))
;;          ))

;; (after! centaur-tabs
;;   (setq! centaur-tabs-adjust-buffer-order 'left)
;;   (pushnew! centaur-tabs-excluded-prefixes "*doom" "*compilation"))

;;; tame evil-textobj-anyblock
(map! :textobj "B" #'evil-inner-curly #'evil-a-curly
      :textobj "C-S-B" #'evil-textobj-anyblock-inner-block #'evil-textobj-anyblock-a-block
 )

;;; tweak Man-mode
(defun ~man-section-search-backwards (&optional print-help)
  "Search backward for the nearest line with Man-overstrike face at first non-whitespace."
  (interactive "p")

  (progn
    (let ((res nil)
          (start-pos (point)))
      (when (eq -1 (forward-line -1))
        (goto-char pos)))

    (catch 'found
      (while (not (bobp))
        (beginning-of-line)
        (let ((pos (point)))
          (skip-chars-forward " \t")
          (when (and (eq (get-text-property (point) 'face) 'Man-overstrike)
                     (save-excursion
                       (let ((case-fold-search nil))
                         (or (re-search-forward "\\W"        (+ 1 (point)) t)
                             (re-search-forward "[A-Z][A-Z]" (+ 2 (point)) t)
                             ))))
            (let ((res-start (point))
                  (res-end (next-single-property-change (point) 'face)))
              ;; (message "Setting match-data to '(%s %s)" res-start res-end)
              (set-match-data (list res-start res-end (current-buffer)))
              )
            (throw 'found t)))
        (forward-line -1))

      (when print-help
        (message "No Man-overstrike line found"))
      (message "here")
      (set-match-data nil)
      nil
      ))
)

(defun ~//man-set-imenu-generic-expression ()
  (setq imenu-generic-expression
        (list (list nil #'~man-section-search-backwards 0)))
  )

;; This sometimes adds too many things to imenu (e.g. on the manpage of bash),
;; but it's an improvement compared to only listing uppercase sections.
(add-hook! Man-mode
           #'~//man-set-imenu-generic-expression)

;; NOTE A fix for a BUG in... dirvish? doom?
(after! dirvish
        (advice-remove #'dired-find-file #'dirvish-find-entry-a))

;; My packages

(use-package! evil-lisp-state
  :config
  (setq! evil-lisp-state-cursor '("salmon" box))
  (evil-lisp-state-leader "H-,")
  (map! :map evil-lisp-state-map
        "u" #'evil-undo
        "C-r" #'evil-redo
        ))

;; (use-package! emacsql)

;; text-obj-anyblock sometimes still does crazy things
;; (setq evil-textobj-anyblock-blocks
;;         '(("(" . ")")
;;           ("{" . "}")
;;           ("\\[" . "\\]")))

;; NOTE Experiments.
;; (load! "+exp/override-doom-leaders")

(map! :leader "f M-f" #'consult-find)

(defvar ~last-compilation-cmd-type nil)

(defun ~compile-project (prefix)
  (interactive "P")
  (cond
   ((equal prefix '(16))
    (call-interactively #'projectile-compile-project)
    (setf ~last-compilation-cmd-type 'projectile-compile))
   ((and (not prefix) (doom-project-p))
      (makefile-executor-execute-project-target)
      (setf ~last-compilation-cmd-type nil))
   (t
    (let ((makefile (cl-loop with buffer-file = (or buffer-file-name default-directory)
                             for file in (list "Makefile" "makefile")
                             for dir = (locate-dominating-file buffer-file file)
                             when dir return (file-name-concat dir file))))
      (unless makefile
        (user-error "Cannot find a makefile in the current project"))
      (let ((default-directory (file-name-directory makefile)))
        (makefile-executor-execute-target makefile)))
    (setf ~last-compilation-cmd-type nil))
   ))

(defun ~repeat-compile-project ()
  (interactive)
  (pcase ~last-compilation-cmd-type
    ('projectile-compile
     (call-interactively #'projectile-repeat-last-command))
    ('nil
     (call-interactively #'+make/run-last)))
  )

(map! "<f2>" #'org-roam-node-find
      "<f3>" #'~compile-project
      "<f4>" #'~repeat-compile-project
      "<f9>" #'doom/open-private-config
      )

(defun ~evil-ex-clear-highlights ()
  (interactive)
  (evil-ex-nohighlight)
  ;; I ripped this code from `evil-ex-search-stop-session'.
  ;; TODO I wonder if this can break something...
  (when evil-ex-search-overlay
    (delete-overlay evil-ex-search-overlay)
    (setf evil-ex-search-overlay nil)
    )
  )
(map! (:localleader
       :map (emacs-lisp-mode-map lisp-mode-map)
       "," #'evil-lisp-state)
      "C-l" #'~evil-ex-clear-highlights
      "C-S-l" #'recenter-top-bottom
      :nv "M-;" #'evilnc-comment-operator
      )

;; See https://git.sr.ht/~meow_king/typst-ts-mode.
(use-package! typst-ts-mode
  :defer t
  :config
  (add-to-list 'auto-mode-alist '("\\.typ" . typst-ts-mode))
  :custom
  (typst-ts-mode-watch-options "--open")
  )
