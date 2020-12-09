;; -*- mode: emacs-lisp; lexical-binding: t -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Layer configuration:
This function should only modify configuration layer settings."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs

   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused

   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t

   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()

   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press `SPC f e R' (Vim style) or
     ;; `M-m f e R' (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     ;; auto-completion
     ;; better-defaults
     ;; markdown
     ;; spell-checking
     ;; syntax-checking
     ;; version-control
     ;; origami
     helm
     (org :variables
          org-enable-reveal-js-support t
          org-enable-org-journal-support t
          org-enable-roam-support t)

     bibtex
     pdf

     git
     lsp
     (shell :variables
            shell-default-position 'bottom)

     emacs-lisp
     common-lisp
     coq
     haskell
     (python :variables python-backend 'anaconda)
     javascript
     racket
     kotlin

     yaml
     markdown
     html

     ;; (scala :variables
     ;;        scala-backend 'scala-metals)

     ;; bespoke!
     bespoke-scala-mode
     bespoke-org-roam
     dotty
     bespoke
     )

   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   ;; To use a local version of a package, use the `:location' property:
   ;; '(your-package :location "~/path/to/your-package/")
   ;; Also include the dependencies as they will not be resolved automatically.
   dotspacemacs-additional-packages
   '((om :location local)
		 org-super-agenda
     yequake
     edit-server
     )

   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()

   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages
   '(
     org-bullets
     evil-escape
     )
   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and deletes any unused
   ;; packages as well as their unused dependencies. `used-but-keep-unused'
   ;; installs only the used packages but won't delete unused ones. `all'
   ;; installs *all* packages supported by Spacemacs and never uninstalls them.
   ;; (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non-nil then enable support for the portable dumper. You'll need
   ;; to compile Emacs 27 from source following the instructions in file
   ;; EXPERIMENTAL.org at to root of the git repository.
   ;; (default nil)
   dotspacemacs-enable-emacs-pdumper nil

   ;; Name of executable file pointing to emacs 27+. This executable must be
   ;; in your PATH.
   ;; (default "emacs")
   dotspacemacs-emacs-pdumper-executable-file "emacs"

   ;; Name of the Spacemacs dump file. This is the file will be created by the
   ;; portable dumper in the cache directory under dumps sub-directory.
   ;; To load it when starting Emacs add the parameter `--dump-file'
   ;; when invoking Emacs 27.1 executable on the command line, for instance:
   ;;   ./emacs --dump-file=$HOME/.emacs.d/.cache/dumps/spacemacs-27.1.pdmp
   ;; (default (format "spacemacs-%s.pdmp" emacs-version))
   dotspacemacs-emacs-dumper-dump-file (format "spacemacs-%s.pdmp" emacs-version)

   ;; If non-nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t

   ;; Maximum allowed time in seconds to contact an ELPA repository.
   ;; (default 5)
   dotspacemacs-elpa-timeout 5

   ;; Set `gc-cons-threshold' and `gc-cons-percentage' when startup finishes.
   ;; This is an advanced option and should not be changed unless you suspect
   ;; performance issues due to garbage collection operations.
   ;; (default '(100000000 0.1))
   dotspacemacs-gc-cons '(100000000 0.1)

   ;; Set `read-process-output-max' when startup finishes.
   ;; This defines how much data is read from a foreign process.
   ;; Setting this >= 1 MB should increase performance for lsp servers
   ;; in emacs 27.
   ;; (default (* 1024 1024))
   dotspacemacs-read-process-output-max (* 1024 1024)

   ;; If non-nil then Spacelpa repository is the primary source to install
   ;; a locked version of packages. If nil then Spacemacs will install the
   ;; latest version of packages from MELPA. Spacelpa is currently in
   ;; experimental state please use only for testing purposes.
   ;; (default nil)
   dotspacemacs-use-spacelpa nil

   ;; If non-nil then verify the signature for downloaded Spacelpa archives.
   ;; (default t)
   dotspacemacs-verify-spacelpa-archives t

   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil

   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'. (default 'emacs-version)
   dotspacemacs-elpa-subdirectory 'emacs-version

   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim

   ;; If non-nil show the version string in the Spacemacs buffer. It will
   ;; appear as (spacemacs version)@(emacs version)
   ;; (default t)
   dotspacemacs-startup-buffer-show-version t

   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official

   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))

   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t

   ;; Default major mode for a new empty buffer. Possible values are mode
   ;; names such as `text-mode'; and `nil' to use Fundamental mode.
   ;; (default `text-mode')
   dotspacemacs-new-empty-buffer-major-mode 'text-mode

   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode

   ;; If non-nil, *scratch* buffer will be persistent. Things you write down in
   ;; *scratch* buffer will be saved and restored automatically.
   dotspacemacs-scratch-buffer-persistent nil

   ;; If non-nil, `kill-buffer' on *scratch* buffer
   ;; will bury it instead of killing.
   dotspacemacs-scratch-buffer-unkillable nil

   ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message nil

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(spacemacs-dark
                         spacemacs-light)

   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `doom', `vim-powerline' and `vanilla'. The
   ;; first three are spaceline themes. `doom' is the doom-emacs mode-line.
   ;; `vanilla' is default Emacs mode-line. `custom' is a user defined themes,
   ;; refer to the DOCUMENTATION.org for more info on how to create your own
   ;; spaceline theme. Value can be a symbol or list with additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   dotspacemacs-mode-line-theme '(spacemacs :separator wave :separator-scale 1.5)

   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Default font or prioritized list of fonts.
   dotspacemacs-default-font '("Source Code Pro"
                               :size 10.0
                               :weight normal
                               :width normal)

   ;; The leader key (default "SPC")
   dotspacemacs-leader-key "SPC"

   ;; The key used for Emacs commands `M-x' (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"

   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"

   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"

   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","

   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m" for terminal mode, "<M-return>" for GUI mode).
   ;; Thus M-RET should work as leader key in both GUI and terminal modes.
   ;; C-M-m also should work in terminal mode, but not in GUI mode.
   dotspacemacs-major-mode-emacs-leader-key (if window-system "<M-return>" "C-M-m")

   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil

   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"

   ;; If non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil

   ;; If non-nil then the last auto saved layouts are resumed automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil

   ;; If non-nil, auto-generate layout name when creating new layouts. Only has
   ;; effect when using the "jump to layout by number" commands. (default nil)
   dotspacemacs-auto-generate-layout-names nil

   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1

   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache

   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5

   ;; If non-nil, the paste transient-state is enabled. While enabled, after you
   ;; paste something, pressing `C-j' and `C-k' several times cycles through the
   ;; elements in the `kill-ring'. (default nil)
   dotspacemacs-enable-paste-transient-state nil

   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4

   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom

   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil

   ;; If non-nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t

   ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil

   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil

   ;; If non-nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup t

   ;; If non-nil the frame is undecorated when Emacs starts up. Combine this
   ;; variable with `dotspacemacs-maximized-at-startup' in OSX to obtain
   ;; borderless fullscreen. (default nil)
   dotspacemacs-undecorated-at-startup nil

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90

   ;; If non-nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t

   ;; If non-nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t

   ;; If non-nil unicode symbols are displayed in the mode line.
   ;; If you use Emacs as a daemon and wants unicode characters only in GUI set
   ;; the value to quoted `display-graphic-p'. (default t)
   dotspacemacs-mode-line-unicode-symbols t

   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t

   ;; Control line numbers activation.
   ;; If set to `t', `relative' or `visual' then line numbers are enabled in all
   ;; `prog-mode' and `text-mode' derivatives. If set to `relative', line
   ;; numbers are relative. If set to `visual', line numbers are also relative,
   ;; but lines are only visual lines are counted. For example, folded lines
   ;; will not be counted and wrapped lines are counted as multiple lines.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :visual nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; When used in a plist, `visual' takes precedence over `relative'.
   ;; (default nil)
   dotspacemacs-line-numbers 'visual

   ;; Code folding method. Possible values are `evil', `origami' and `vimish'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil

   ;; If non-nil `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil

   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc...
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil

   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all

   ;; If non-nil, start an Emacs server if one is not already running.
   ;; (default nil)
   dotspacemacs-enable-server t

   ;; Set the emacs server socket location.
   ;; If nil, uses whatever the Emacs default is, otherwise a directory path
   ;; like \"~/.emacs.d/server\". It has no effect if
   ;; `dotspacemacs-enable-server' is nil.
   ;; (default nil)
   dotspacemacs-server-socket-dir nil

   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil

   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")

   ;; Format specification for setting the frame title.
   ;; %a - the `abbreviated-file-name', or `buffer-name'
   ;; %t - `projectile-project-name'
   ;; %I - `invocation-name'
   ;; %S - `system-name'
   ;; %U - contents of $USER
   ;; %b - buffer name
   ;; %f - visited file name
   ;; %F - frame name
   ;; %s - process status
   ;; %p - percent of buffer above top of window, or Top, Bot or All
   ;; %P - percent of buffer above bottom of window, perhaps plus Top, or Bot or All
   ;; %m - mode name
   ;; %n - Narrow if appropriate
   ;; %z - mnemonics of buffer, terminal, and keyboard coding systems
   ;; %Z - like %z, but including the end-of-line format
   ;; (default "%I@%S")
   dotspacemacs-frame-title-format "%I@%S"

   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil

   ;; If non nil activate `clean-aindent-mode' which tries to correct
   ;; virtual indentation of simple modes. This can interfer with mode specific
   ;; indent handling like has been reported for `go-mode'.
   ;; If it does deactivate it here.
   ;; (default t)
   dotspacemacs-use-clean-aindent-mode t

   ;; If non-nil shift your number row to match the entered keyboard layout
   ;; (only in insert state). Currently supported keyboard layouts are:
   ;; `qwerty-us', `qwertz-de' and `querty-ca-fr'.
   ;; New layouts can be added in `spacemacs-editing' layer.
   ;; (default nil)
   dotspacemacs-swap-number-row nil

   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil

   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs nil

   ;; If nil the home buffer shows the full path of agenda items
   ;; and todos. If non nil only the file name is shown.
   dotspacemacs-home-shorten-agenda-source nil))

(defun dotspacemacs/user-env ()
  "Environment variables setup.
This function defines the environment variables for your Emacs session. By
default it calls `spacemacs/load-spacemacs-env' which loads the environment
variables declared in `~/.spacemacs.env' or `~/.spacemacs.d/.spacemacs.env'.
See the header of this file for more information."
  (spacemacs/load-spacemacs-env))

(defun dotspacemacs/user-init ()
  "Initialization for user code:
This function is called immediately after `dotspacemacs/init', before layer
configuration.
It is mostly for variables that should be set before packages are loaded.
If you are unsure, try setting them in `dotspacemacs/user-config' first."

  (spacemacs|use-package-add-hook racket-mode
    :post-config
    (define-key racket-mode-map (kbd "<f5>") nil)
    (remove-hook 'racket-mode-hook #'racket-xp-mode))

  (spacemacs|use-package-add-hook kotlin-mode
    :post-config
    (remove-hook 'kotlin-mode-hook #'spacemacs//kotlin-setup-backend))

  (spacemacs|use-package-add-hook scala-mode
    :post-config
    (setq scala-auto-insert-asterisk-in-comments t)
    (add-to-list 'spacemacs-indent-sensitive-modes 'scala-mode)
    (remove-hook 'scala-mode-hook #'lsp))

  (setq which-key-enable-extended-define-key t)
  )

(defun dotspacemacs/user-load ()
  "Library to load while dumping.
This function is called only while dumping Spacemacs configuration. You can
`require' or `load' the libraries of your choice that will be included in the
dump."
  )

(defun dotspacemacs/user-config ()
  "Configuration for user code:
This function is called at the very end of Spacemacs startup, after layer
configuration.
Put your configuration code here, except for variables that should be set
before packages are loaded."

  (use-package smartparens) ;; sometimes the smartparens self-insert hook inexplicably doesn't get inserted, manually using it fixes that
  (use-package org-ml) ;; can't use :defer, since org-mode is loaded by default
  (use-package ox-md) ;; fixes md export being unavailable in Org menu
	(use-package org-super-agenda
    :config
    (org-super-agenda-mode 1)
    (defun my-super-agenda/go ()
      (interactive)
      (let* ((work-categories (list "Dotty" "doctool"))
             (org-super-agenda-groups
              `((:name "Work (TODO)"
                       :and (:category ,work-categories :todo ("TODO")))
                (:name "Work (TASK)"
                       :and (:category ,work-categories :todo ("TASK")))
                (:name "Work (Problems)"
                       :and (:category ,work-categories :todo ("OPEN")))
                )))
        (org-agenda nil "t"))
      ))

  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)

  (load "~/.spacemacs.d/bespoke.el")

  ;; (org-roam-mode 1)

  ;;; configuration

  (progn ;;emacs
    (setq create-lockfiles nil)
    )

  (progn ;; spacemacs
    (setq
     spacemacs-layouts-restrict-spc-tab t)

    (electric-indent-mode nil)

    (defun my-spacemacs//yank-ident-region (yank-func &rest args)
      "If current mode is not one of spacemacs-indent-sensitive-modes
indent yanked text (with universal arg don't indent)."
      (evil-start-undo-step)
      (prog1
          (let ((prefix current-prefix-arg)
                (enable (and (not (member major-mode spacemacs-indent-sensitive-modes))
                             (or (derived-mode-p 'prog-mode)
                                 (member major-mode spacemacs-yank-indent-modes)))))
            (when (and enable (equal '(4) prefix))
              (setq args (cons 1 (cdr args))))
            (prog1
                (apply yank-func args)
              (when (and enable (not (equal '(4) prefix)))
                (let ((transient-mark-mode nil)
                      (save-undo buffer-undo-list))
                  (spacemacs/yank-advised-indent-function (region-beginning)
                                                          (region-end))))))
        (evil-end-undo-step)))

    (advice-remove #'spacemacs//yank-indent-region
                   #'my-spacemacs//yank-ident-region)

    (advice-add #'spacemacs//yank-indent-region
                :override
                #'my-spacemacs//yank-ident-region))

  (progn ;; dotty
    (defvar-local yas-arg/dotty-current-printer nil)

    (defvar dotty-sbt/known-options)
    (setq dotty-sbt/known-options '("-Yescape-analysis"
                                    "-Yshow-suppressed-errors"
                                    "-Ythrough-tasty"
                                    "-Xprint:typer"
                                    "-Xprint:all"
                                    "-Xprint:lambdaLift"
                                    "-Xprompt"
                                    "-uniqid"))

    (defvar dotty-sbt//arglist-prefix "-color:never -d out")
    (defvar dotty-sbt//set-arguments)
    (setq dotty-sbt//set-arguments '("-Yescape-analysis"))

    (defun dotty-sbt//apply-set-arguments ()
      (let ((argstring (with-temp-buffer
                         (insert dotty-sbt//arglist-prefix)
                         (cl-loop for opt in dotty-sbt//set-arguments
                                  do (progn
                                       (insert " ")
                                       (insert opt)))
                         (buffer-string))))
        (setq sbt/compile-arguments argstring)
        (when (bound-and-true-p helm-alive-p)
          (helm-set-pattern "")
          (helm-update))))

    (defun dotty-sbt/helm-setopt-toggle-option (candidate)
      (interactive)
      (setq dotty-sbt//set-arguments
            (if (-contains? dotty-sbt//set-arguments candidate)
                (-remove-item candidate dotty-sbt//set-arguments)
              (cons candidate dotty-sbt//set-arguments)))
      (dotty-sbt//apply-set-arguments))

    (defun dotty-sbt/helm-pick-opts ()
      (interactive)
      (helm :sources (helm-build-sync-source
                         "Dotty options"
                       :header-name (lambda (n)
                                      (format "%s (current: `%s`)" n sbt/compile-arguments))
                       :candidates dotty-sbt/known-options
                       :action #'dotty-sbt/helm-setopt-toggle-option
                       :persistent-action #'dotty-sbt/helm-setopt-toggle-option
                       )
            :buffer "*helm Dotty options*"))

    (defun my-dotty//configure ()
      (dotty/set-keys)
      (setq sbt/test-command "testCompilation .eff.")
      (dotty-sbt//apply-set-arguments))
    (eval-after-load 'dotty #'my-dotty//configure))

  (progn ;; org-mode
    (setq org-todo-keywords '((sequence "TODO(t)" "DONE(d)")
                              (sequence "TASK(s)" "|")
                              (sequence "OPEN(o)" "CLSD(c)"))
          org-reverse-note-order t
          org-agenda-files "~/.cache/emacs-org-mode/agenda")

    (defun my-org/move-to-notes ()
      (interactive)
      (org-roam-find-file)
      (goto-char (org-element-property :begin
                                       (cl-find-if (lambda (el) (and (org-ml-is-type 'headline el)
                                                                     (string-equal "Notes" (org-ml-get-property :title el))))
                                                   (org-element-parse-buffer 'headline)))))

    (defun my-org/archive-repeating-meeting ()
      (interactive)
      ;; make current date inactive
      (-let [stamp (->> (org-ml-parse-this-headline)
                        (org-ml-get-property :title)
                        (car))]
        (unless (eq 'timestamp (org-ml-get-type stamp))
          (user-error "Headline at point doesn't have a timestamp"))
        (org-ml-update (lambda (stamp)
                         (org-ml-timestamp-set-active nil stamp))
                       stamp))
      ;; refile the meeting
      (if-let ((target (let ((org-refile-targets '((nil . (:tag . "past")))))
                         (cl-find-if (lambda (it)
                                       (> (nth 3 it) (point)))
                                     (org-refile-get-targets)))))
          (org-refile nil nil target "Make it part of the past")
        (user-error "Could not find a target to refile to"))
      )

    (defun my-org/duplicate-repeating-meeting ()
      (interactive)
      (save-excursion
        (-let [line (buffer-substring (line-beginning-position)
                                      (line-end-position))]
          (forward-line -1)
          (insert line)))
      ;; move next date into the future
      (save-excursion
        (forward-line -1)
        (-let [stamp (->> (org-ml-parse-this-headline)
                          (org-ml-get-property :title)
                          (car))]
          (unless (eq 'timestamp (org-ml-get-type stamp))
            (user-error "Headline at point doesn't have a timestamp"))
          (let ((shift-value (org-ml-get-property :repeater-value stamp))
                (shift-unit (org-ml-get-property :repeater-unit stamp)))
            (unless (and shift-value shift-unit)
              (user-error "Timestamp doesn't have a repeater"))
            (org-ml-update (lambda (stamp)
                             (org-ml-timestamp-shift shift-value shift-unit stamp))
                           stamp))))))

  (progn ;; org-ref
    ;; NOTE: bibliography is supposed to be exported from Zotero with better-bibtex
    (setq org-ref-default-bibliography '("~/.cache/zotero-export/PhD.bib")
          org-ref-pdf-directory "~/zotero-pdf/"
          org-ref-bibliography-notes "~/org/bibliography.org"
          org-ref-show-citation-on-enter nil)
    (setq reftex-default-bibliography org-ref-default-bibliography)
    (setq bibtex-completion-library-path org-ref-pdf-directory)
   )

  (progn ;; org-journal
    (setq org-journal-dir "~/org/journal"
          org-journal-file-type 'weekly))

  (progn ;; shell
    (setq shell-default-shell 'eshell)
    )

  (progn ;; lsp
    (setq lsp-signature-auto-activate nil
          lsp-prefer-flymake nil

        ;;; lsp-ui
          lsp-ui-doc-enable nil
          lsp-ui-flycheck-enable t
          lsp-ui-flycheck-live-reporting nil)

    (defvar my-lsp//forced 'nil)
    (defun my-lsp//advice-defang-lsp (f &rest r)
      (if my-lsp//forced (apply f r) (message "Defanged LSP.")))
    (advice-add 'lsp :around #'my-lsp//advice-defang-lsp)
    (defun my-lsp (arg)
      (interactive "p")
      (let ((my-lsp//forced t))
        (lsp arg)))
    )

  (progn ;; flycheck
    (setq flycheck-check-syntax-automatically '(save mode-enabled)))

  ;;; fun

  (defun my-sbt/abort ()
    (interactive)
    (with-current-buffer "*sbt<dotty>*"
      (comint-interrupt-subjob)))

  ;;; keybindings

  (define-key key-translation-map
    (kbd "H-,") (kbd dotspacemacs-major-mode-emacs-leader-key))

  (define-key key-translation-map
    (kbd "H-SPC") (kbd dotspacemacs-emacs-leader-key))

  (global-set-key (kbd "H-1") #'eyebrowse-switch-to-window-config-1)
  (global-set-key (kbd "H-2") #'eyebrowse-switch-to-window-config-2)
  (global-set-key (kbd "H-3") #'eyebrowse-switch-to-window-config-3)
  (global-set-key (kbd "H-4") #'eyebrowse-switch-to-window-config-4)

  (global-set-key (kbd "<C-tab>") #'spacemacs/alternate-window)

  (global-set-key (kbd "<f1>") #'spacemacs/persp-switch-to-1)
  (global-set-key (kbd "<f2>") #'my-perspective/switch-to-para)
  (global-set-key (kbd "<f3>") #'my-perspective/switch-to-bespoke)
  (global-set-key (kbd "<f4>") #'my-perspective/switch-to-dotty)

  (global-set-key (kbd "<f5>") #'my-perspective/switch-to-dynamic)
  (global-set-key (kbd "<f6>") #'my-perspective/switch-to-dynamic)
  (global-set-key (kbd "<f7>") #'my-perspective/switch-to-dynamic)
  (global-set-key (kbd "<f8>") #'my-perspective/switch-to-dynamic)

  (evil-define-key 'normal 'global
    "[-" 'my/backwards-jump-to-outdent
    "]-" 'my/forwards-jump-to-outdent
    "[=" 'my/backwards-jump-to-same-indent
    "]=" 'my/forwards-jump-to-same-indent
    "[+" 'my/backwards-jump-to-indent
    "]+" 'my/forwards-jump-to-indent)

  (evil-define-key 'motion 'global
    "]a" 'evil-forward-arg
    "[a" 'evil-backward-arg
    "[A" 'evil-jump-out-args
    "]j" 'my/slurp)

  (evil-define-key '(normal insert) org-mode-map
    (kbd "<H-return>") (lookup-key spacemacs-org-mode-map "i")
    (kbd "H-,") (kbd "ESC SPC m"))

  (spacemacs/set-leader-keys
    "gd" #'my/magit/kill-all-buffers
    "ps" #'my/projectile/save-project-files
    "rh" #'my/help-resume
    "oc" #'my-sbt/abort
    "oa" #'my-super-agenda/go)

  (spacemacs/set-leader-keys-for-major-mode 'org-mode
    "i <f2>" #'org-roam-insert
    "oi" #'my/org/set-ztk-id
    "os" #'my/org/sort-todos
    "oc" #'my-org/set-created
    "o1" #'my-org/duplicate-repeating-meeting
    "o2" #'my-org/archive-repeating-meeting)

  (spacemacs/set-leader-keys-for-major-mode 'lisp-mode
    "," #'lisp-state-toggle-lisp-state)

  ;; NOTE to avoid stupid functions for setting values of custom settings, find a good way of setting them here
  )

(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-want-change-word-to-end nil)
 '(org-capture-templates
   '(("z" "Roam ZTK note" entry
      (file+olp buffer-file-name "ZTK" "Niezorganizowane")
      "* #? %?
  :PROPERTIES:%(org-cycle)
  :ID:       %(org-id-new)
  :ZTK:      ?
  :END:" :unnarrowed t :no-save t)
     ("r" "Roam reading list" entry
      (file+headline "~/org/roam/do_przeczytania.org" "Notes")
      "* 
  %u
** Dlaczego?
" :prepend t :empty-lines 1)
     ("c" "Roam capture note" entry
      (file "~/org/roam/captured.org")
      "* ")
     ("n" "Roam note" entry #'my-org/move-to-notes "* %?\n%a")
     ("g" "Note" entry
      (file "~/org/notes.org")
      "")
     ("t" "Project TODO" entry
      (file+headline my/current-project-TODOs-file "TODOs")
      #'my/org-template/project-todo-capture)))
 '(package-selected-packages
   '(edit-server yequake proof-general company-coq company-math math-symbol-lists merlin-eldoc stickyfunc-enhance helm-gtags helm-cscope xcscope ggtags counsel-gtags counsel swiper org-pdftools org-noter-pdftools org-noter org-roam-bibtex dap-mode posframe bui sbt-mode tide typescript-mode kotlin-mode flycheck-kotlin tern org-roam pdf-tools org-journal origami yapfify yaml-mode xterm-color vterm utop tuareg caml terminal-here shell-pop seeing-is-believing rvm ruby-tools ruby-test-mode ruby-refactor ruby-hash-syntax rubocopfmt rubocop rspec-mode robe rjsx-mode rbenv rake pytest pyenv-mode py-isort pippel pipenv pyvenv pip-requirements ocp-indent ob-elixir mvn multi-term minitest meghanada maven-test-mode lsp-python-ms lsp-java live-py-mode importmagic epc ctable concurrent deferred helm-pydoc groovy-mode groovy-imports pcache gradle-mode git-gutter-fringe+ fringe-helper git-gutter+ flycheck-ocaml merlin flycheck-mix flycheck-credo eshell-z eshell-prompt-extras esh-help emojify emoji-cheat-sheet-plus dune cython-mode company-emoji company-anaconda chruby bundler inf-ruby browse-at-remote blacken auto-complete-rst anaconda-mode pythonic alchemist elixir-mode smeargle orgit magit-gitflow magit-popup helm-gitignore gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link evil-magit git-commit with-editor web-mode tagedit slim-mode scss-mode sass-mode pug-mode less-css-mode helm-css-scss haml-mode emmet-mode ox-reveal web-beautify livid-mode skewer-mode simple-httpd json-mode json-snatcher json-reformat js2-refactor yasnippet multiple-cursors js2-mode js-doc coffee-mode scala-mode mmm-mode markdown-toc markdown-mode gh-md org-projectile org-category-capture org-present org-pomodoro alert log4e gntp org-mime org-download lv htmlize gnuplot racket-mode faceup slime-company company slime ws-butler winum which-key volatile-highlights vi-tilde-fringe uuidgen use-package toc-org spaceline powerline restart-emacs request rainbow-delimiters popwin persp-mode pcre2el paradox spinner org-plus-contrib org-bullets open-junk-file neotree move-text macrostep lorem-ipsum linum-relative link-hint indent-guide hydra hungry-delete hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation helm-themes helm-swoop helm-projectile helm-mode-manager helm-make projectile pkg-info epl helm-flx helm-descbinds helm-ag google-translate golden-ratio flx-ido flx fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist highlight evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-args evil-anzu anzu evil goto-chg undo-tree eval-sexp-fu elisp-slime-nav dumb-jump f dash s diminish define-word column-enforce-mode clean-aindent-mode bind-map bind-key auto-highlight-symbol auto-compile packed aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line helm avy helm-core popup async))
 '(yequake-frames
   '(("Scratch"
      (buffer-fns "*scratch*")
      (width . 1.0)
      (height . 1.0)
      (left . 0)
      (top . 0)
      (frame-parameters
       (skip-taskbar . t)
       (sticky . t)
       (undecorated . t))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
)



