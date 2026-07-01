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

;; Package guard helpers
(defun ~straight-package-local-repo (package)
  "Return straight's local repo name for PACKAGE."
  (let* ((package-name (if (symbolp package) (symbol-name package) package))
         (package-symbol (if (symbolp package) package (intern package-name)))
         (recipe (when (fboundp 'straight-recipes-retrieve)
                   (ignore-errors
                     (straight-recipes-retrieve package-symbol)))))
    (or (and recipe
             (fboundp 'straight-vc-local-repo-name)
             (ignore-errors
               (straight-vc-local-repo-name recipe)))
        package-name)))

(defun ~assert-straight-package-commit (package expected-commit &optional context)
  "Signal an error unless PACKAGE is checked out at EXPECTED-COMMIT.

CONTEXT should describe the config or monkey patch that depends on the exact
package revision."
  (require 'straight)
  (unless (fboundp 'straight-vc-get-commit)
    (error "Cannot verify straight commit for %S%s"
           package
           (if context (format " (%s)" context) "")))
  (let* ((local-repo (~straight-package-local-repo package))
         (actual-commit (straight-vc-get-commit 'git local-repo)))
    (unless (equal actual-commit expected-commit)
      (error "%S is at %s, but%s expected %s; review the dependent config"
             package
             actual-commit
             (if context (format " %s" context) "")
             expected-commit))))

;; Emacs server config
(defun ~target-window-for-buffer (buf)
  "Return a visible window suitable for displaying BUF.

Prefer a visible frame whose current perspective already contains BUF. If no
such frame exists, prefer a visible window already showing BUF on another frame."
  (require 'cl-lib)
  (let* (;; All windows showing BUF on any visible frame.
         (visible-wins (get-buffer-window-list buf 'no-minibuf 'visible))
         ;; Prefer selecting an existing window on some *other* frame first, so we
         ;; don't keep jumping to the window `server.el' just created.
         (other-frame-win
          (cl-find-if (lambda (w)
                        (not (eq (window-frame w) (selected-frame))))
                      visible-wins))
         ;; If BUF is in a perspective and some other visible frame is currently
         ;; on that perspective, use that frame.
         (persps (when (fboundp 'persp--buffer-in-persps)
                   (delq nil (persp--buffer-in-persps buf))))
         (persp-frame
          (when (and persps (fboundp 'get-frame-persp))
            (or (cl-find-if (lambda (f)
                              (and (frame-live-p f)
                                   (eq (frame-visible-p f) t)
                                   (not (eq f (selected-frame)))
                                   (memq (get-frame-persp f) persps)))
                            (frame-list))
                (cl-find-if (lambda (f)
                              (and (frame-live-p f)
                                   (eq (frame-visible-p f) t)
                                   (memq (get-frame-persp f) persps)))
                            (frame-list))))))
    (cond
     ;; If we found a frame matching BUF's persp, prefer a window in that frame.
     (persp-frame
      (or (cl-find-if (lambda (w)
                        (eq (window-frame w) persp-frame))
                      visible-wins)
          (let ((w (frame-selected-window persp-frame)))
            (and (window-live-p w)
                 (not (window-minibuffer-p w))
                 (not (window-dedicated-p w))
                 w))
          (cl-find-if (lambda (w)
                        (and (not (window-minibuffer-p w))
                             (not (window-dedicated-p w))))
                      (window-list persp-frame 'no-minibuf))))
     (other-frame-win other-frame-win)
     (t (car visible-wins)))))

(defun ~select-window-for-buffer (buf)
  "Display BUF in a target window and select it."
  (when-let ((target-win (~target-window-for-buffer buf)))
    (let ((frame (window-frame target-win)))
      (when (and (frame-live-p frame)
                 (not (eq frame (selected-frame))))
        (select-frame-set-input-focus frame))
      (select-window target-win)
      (unless (eq (current-buffer) buf)
        (switch-to-buffer buf nil t))
      target-win)))

(defun ~server-select-window-where-buffer-is-visible ()
  "Switch to an existing frame/window for the current server buffer.

Intended to be called from `server-switch-hook'.
My use-case is improving the behavior when writing Latex and jumping from preview to source.
If I was doing something else in another Emacs frame,
I come back to the preview and I jump to the source from the preview,
the buffer gets opened in the last used frame,which is never what I want.

If the buffer is already assigned to a perspective (persp-mode / Doom workspaces),
and there's a visible frame whose current perspective matches, prefer that frame
even if the buffer isn't currently visible there."
  (~select-window-for-buffer (current-buffer)))

(after! server
  (add-hook! server-switch #'~server-select-window-where-buffer-is-visible))

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

;; FOR TEXT MESSAGES TO BE COPIED SOMEWHERE ELSE
(defun ~join-paragraph ()
  "Join lines in current paragraph into one line, removing end-of-lines.
With active region, join lines in all paragraphs inside the region."
  (interactive)
  ;; Code ripped from rst-join-paragraph.
  (let ((fill-column 65000)) ; Some big number.
    (call-interactively #'fill-paragraph)))

(map! :nv "g M-J" #'~join-paragraph)
