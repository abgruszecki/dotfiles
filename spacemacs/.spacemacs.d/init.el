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
     ;; spell-checking
     ;; syntax-checking
     ;; version-control
     ;; origami
     helm
     (org :variables
          org-enable-github-support t
          org-enable-reveal-js-support t
          org-enable-org-journal-support t
          org-enable-roam-support t)
     eww
     bibtex
     pdf

     git
     lsp
     (shell :variables
            shell-default-position 'bottom)

     shell-scripts
     emacs-lisp
     common-lisp
     coq
     haskell
     (python :variables python-backend 'anaconda)
     javascript
     racket
     kotlin

     ipython-notebook

     csv
     yaml

     (latex :variables latex-backend 'company-auctex)
     markdown
     html

     (scala :variables
            scala-backend 'scala-metals
            scala-auto-treeview nil)

     ;; bespoke!
     bespoke-scala-mode
     bespoke-org-roam
     dotty
     bespoke
     )

   ;; List of additional packages that will be installed without being wrapped
   ;; in a layer (generally the packages are installed only and should still be
   ;; loaded using load/require/use-package in the user-config section below in
   ;; this file). If you need some configuration for these packages, then
   ;; consider creating a layer. You can also put the configuration in
   ;; `dotspacemacs/user-config'. To use a local version of a package, use the
   ;; `:location' property: '(your-package :location "~/path/to/your-package/")
   ;; Also include the dependencies as they will not be resolved automatically.
   dotspacemacs-additional-packages
   '((om :location local)
     org-noter
     org-noter-pdftools
		 org-super-agenda
     general
     yequake
     edit-server
     )

   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()

   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages
   '(org-bullets
     evil-escape
     yasnippet
     auto-yasnippet
     yasnippet-snippets
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
   ;; `recents' `recents-by-project' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   ;; The exceptional case is `recents-by-project', where list-type must be a
   ;; pair of numbers, e.g. `(recents-by-project . (7 .  5))', where the first
   ;; number is the project limit and the second the limit on the recent files
   ;; within a project.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))

   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t

   ;; Show numbers before the startup list lines. (default t)
   dotspacemacs-show-startup-list-numbers t

   ;; The minimum delay in seconds between number key presses. (default 0.4)
   dotspacemacs-startup-buffer-multi-digit-delay 0.4

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

   ;; Default font or prioritized list of fonts. The `:size' can be specified as
   ;; a non-negative integer (pixel size), or a floating-point (point size).
   ;; Point size is recommended, because it's device independent. (default 10.0)
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

   ;; Show the scroll bar while scrolling. The auto hide time can be configured
   ;; by setting this variable to a number. (default t)
   dotspacemacs-scroll-bar-while-scrolling nil

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

   ;; If non-nil and `dotspacemacs-activate-smartparens-mode' is also non-nil,
   ;; `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil

   ;; If non-nil smartparens-mode will be enabled in programming modes.
   ;; (default t)
   dotspacemacs-activate-smartparens-mode t

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
   ;; If nil then Spacemacs uses default `frame-title-format' to avoid
   ;; performance issues, instead of calculating the frame title by
   ;; `spacemacs/title-prepare' all the time.
   ;; (default "%I@%S")
   dotspacemacs-frame-title-format "%I@%S"

   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil

   ;; Show trailing whitespace (default t)
   dotspacemacs-show-trailing-whitespace t

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup 'changed

   ;; If non nil activate `clean-aindent-mode' which tries to correct
   ;; virtual indentation of simple modes. This can interfer with mode specific
   ;; indent handling like has been reported for `go-mode'.
   ;; If it does deactivate it here.
   ;; (default t)
   dotspacemacs-use-clean-aindent-mode t

   ;; Accept SPC as y for prompts if non nil. (default nil)
   dotspacemacs-use-SPC-as-y t

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
   dotspacemacs-home-shorten-agenda-source nil

   ;; If non-nil then byte-compile some of Spacemacs files.
   dotspacemacs-byte-compile nil))

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
	(use-package org-super-agenda
    :config
    (org-super-agenda-mode 1))

  (use-package org-noter
    :after (:any org pdf-view))

  (use-package org-noter-pdftools
    :after org-noter
    :config
    (with-eval-after-load 'pdf-annot
      (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)))

  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)

  (load "~/.spacemacs.d/bespoke.el")

  (org-roam-mode 1)

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

  (progn ;; perspective
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
        (let ((msg (format "Undead buffer: %s" buf)))
          (message msg)
          (with-current-buffer (get-buffer-create "*persp-trace*")
            (insert msg "\n")))))

    (advice-add #'kill-buffer :around #'bespoke/trace-kill-buffer)
    )

  (progn ;; dotty
    (defvar-local yas-arg/dotty-current-printer nil)

    (defvar dotty-sbt/known-options)
    (setq dotty-sbt/known-options '("-Ydebug-error"
                                    "-Yescape-analysis"
                                    "-Yshow-suppressed-errors"
                                    "-Yno-deep-subtypes"
                                    "-Ythrough-tasty"
                                    "-Ycheck:all"
                                    "-Xprint:typer"
                                    "-Xprint:all"
                                    "-Xprint:lambdaLift"
                                    "-Xprompt"
                                    "-uniqid"))

    (defvar dotty-sbt//arglist-prefix "-color:never -d out")
    (defvar dotty-sbt//set-arguments nil)
    ;; (setq dotty-sbt//set-arguments '("-Yescape-analysis"))

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

  (progn ;; proof-general
    (defun my-pg//make-undo-great-again ()
      (setq-local evil-undo-function #'pg-protected-undo))
    (add-hook 'coq-mode-hook #'my-pg//make-undo-great-again)

    (spacemacs/set-leader-keys-for-major-mode 'coq-mode
      "r" #'proof-retract-current-goal
      )
    )

  (progn ;; org-mode
    (setq org-todo-keywords '((sequence "TODO(t)" "DONE(d)")
                              (sequence "STEP(s)" "DONE(d)")
                              (sequence "TASK(k)" "DONE(d)")
                              (sequence "OPEN(o)" "CLSD(c)")
                              (sequence "EVNT(e)" "PAST(p)"))
          org-reverse-note-order t
          org-agenda-files "~/.cache/emacs-org-mode/agenda"
          org-refile-targets '((org-agenda-files . (:level . 1)))
          org-outline-path-complete-in-steps nil
          org-refile-use-outline-path t
          org-export-with-toc nil
          )

    (setf (alist-get 'system org-file-apps) "xdg-open %s"
          (alist-get "\\.pdf\\'" org-file-apps nil nil #'string=) 'system)

    (add-to-list 'org-babel-load-languages '(ein . t))

    (defvar my-org//todo-capture-template
      "* %(plist-get org-capture-plist :my-org//todo-item) %?
  :PROPERTIES:
  :CREATED: %u
  :END:
")

    (defun my-org//todo-capture-template ()
      my-org//todo-capture-template)

    (defun my-org//advice-after-refile-save-org-buffers (&rest r)
      (org-save-all-org-buffers))

    (advice-add #'org-refile :after #'my-org//advice-after-refile-save-org-buffers)

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

  (progn ;; org-super-agenda
    (setq org-super-agenda-groups
          '((:name "Closed"  :log closed)
            (:name "Clocked" :log clocked)
            (:name "Logged"  :log t)

            (:discard (:todo ("PAST")))

            (:name "Wpisane"
                   :scheduled past
                   :scheduled today
                   ;; :and (:property ("todo-type" (lambda (p) (eq p 'done)))
                   ;;       :not (:not (:scheduled past) :not (:scheduled today))
                   ;;       )
                   )
            (:name "Chwycone (TODO)"
                   :and (:category "Chwycone" :todo ("TODO")))
            (:name "Chwycone (other)"
                   :category "Chwycone")
            (:name "TODOs" :todo ("TODO"))
            (:name "Zadania" :todo ("TASK"))
            (:name "Problemy" :todo ("OPEN"))
            (:name "Inne")
             ))
    )

  (progn ;; shell
    (setq shell-default-shell 'vterm)
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
  (use-package general)

  (defun bespoke-hack/test ()
    (interactive)
    (push (cons 'no-record ?\s) unread-command-events))

  (define-key key-translation-map
    (kbd "H-,") (kbd dotspacemacs-major-mode-emacs-leader-key))

  (general-define-key
   :prefix "H-SPC"
   "SPC" #'bespoke-hack/test
   "TAB" #'eyebrowse-last-window-config
   "ESC" #'yequake-retoggle
   "<f1>" #'my-eyebrowse/toggle-magit)

  (general-define-key
   "H-0" #'eyebrowse-switch-to-window-config-0
   "H-1" #'eyebrowse-switch-to-window-config-1
   "H-2" #'eyebrowse-switch-to-window-config-2
   "H-3" #'eyebrowse-switch-to-window-config-3
   "H-4" #'eyebrowse-switch-to-window-config-4

   "<f1>" #'spacemacs/persp-switch-to-1
   "<f2>" #'my-perspective/switch-to-para
   "<f3>" #'my-perspective/switch-to-bespoke
   "<f4>" #'my-perspective/switch-to-dotty

   "<f5>" #'my-perspective/switch-to-dynamic
   "<f6>" #'my-perspective/switch-to-dynamic
   "<f7>" #'my-perspective/switch-to-dynamic
   "<f8>" #'my-perspective/switch-to-dynamic

   "<f12>" #'org-capture
   "<C-f12>" (lambda () (interactive)
               (my-perspective/switch-to-para)
               (org-agenda))

   "<C-tab>" #'spacemacs/alternate-window

   "<H-tab>" #'eyebrowse-last-window-config
   "<H-escape>" #'my-eyebrowse/toggle-magit)

  (general-define-key
   :keymaps 'emacs-lisp-mode-map
   "H-." #'lisp-state-toggle-lisp-state)

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

  (spacemacs/set-leader-keys "o#" #'sbt/console)
  (spacemacs/set-leader-keys
    "gd" #'my/magit/kill-all-buffers
    "rh" #'my/help-resume
    "oc" #'my-sbt/abort
    "oa" #'my-super-agenda/go
    "ob" #'bespoke-org-ref/top-helm-bibtex
    "oje" #'my-org/jump-to-events
    "oja" #'my-org-roam/jump-to-agenda-file
    )

  (spacemacs/set-leader-keys-for-major-mode 'org-mode
    "i <f2>" #'org-roam-insert
    "i `" #'my-org/insert-code-block
    "oa" #'org-archive-to-archive-sibling
    "oi" #'my/org/set-ztk-id
    "os" #'my-org/sort-todos
    "oc" #'my-org/set-created
    "o1" #'my-org/duplicate-repeating-meeting
    "o2" #'my-org/archive-repeating-meeting
    "C-'" #'dotty/edit-scala3-trace
    )

  (spacemacs/set-leader-keys-for-major-mode 'lisp-mode
    "," #'lisp-state-toggle-lisp-state)

  (spacemacs/set-leader-keys-for-minor-mode 'org-capture-mode
    "C-," #'bespoke-org/capture-finalize-and-jump)

  (setq org-capture-templates
        '(("h" "Log here" entry #'values "* %U
 %?" :prepend t)
          ("z" "Roam ZTK note" entry
           (file+olp buffer-file-name "ZTK" "Niezorganizowane")
           "* #? %?
  :PROPERTIES:%(org-cycle)
  :ID:       %(org-id-new)
  :ZTK:      ?
  :END:" :unnarrowed t :no-save t)
          ("r" "Roam reading list" entry
           (file+headline "~/org/roam/do_przeczytania.org" "Notes")
           "* %?
  %u
** Dlaczego?
" :prepend t :empty-lines 1)
          ("l" "Log research" entry
           (file+headline "~/org/research-log.org" "Log")
           "** %U %?
  %a" :prepend t)
          ("e" "Events")
          ("ee" "Event NOW" entry
           (file+olp "~/org/roam/captured.org" "Wydarzenia")
           "*** EVNT %?\nSCHEDULED: %T"
           :clock-in t
           :prepend t)
          ("e1" "Dotty weekly item" entry
           (file+olp "~/org/roam/captured.org" "Wydarzenia" "Dotty weekly")
           "*** %?")
          ("c" "Todo item")
          ("c1" "Todo item (TODO)" entry
           (file+headline "~/org/roam/captured.org" "TODOs")
           #'my-org//todo-capture-template :my-org//todo-item "TODO" :prepend t :jump-to-captured nil)
          ("c2" "Todo item (TASK)" entry
           (file+headline "~/org/roam/captured.org" "Zadania")
           #'my-org//todo-capture-template :my-org//todo-item "TASK" :prepend t)
          ("c3" "Todo item (OPEN)" entry
           (file+headline "~/org/roam/captured.org" "Problemy")
           #'my-org//todo-capture-template :my-org//todo-item "OPEN" :prepend t)
          ("C" "Immediate todo item")
          ("C1" "Immediate todo item (TODO)" entry
           (file+headline buffer-file-name "TODOs")
           #'my-org//todo-capture-template :my-org//todo-item "TODO" :prepend t)
          ("C2" "Immediate todo item (TASK)" entry
           (file+headline buffer-file-name "Zadania")
           #'my-org//todo-capture-template :my-org//todo-item "TASK" :prepend t)
          ("C3" "Immediate todo item (OPEN)" entry
           (file+headline buffer-file-name "Problemy")
           #'my-org//todo-capture-template :my-org//todo-item "OPEN" :prepend t)
          ("n" "Roam note" entry #'my-org/move-to-notes "* %?
%a")
          ("g" "Note" entry
           (file+headline "~/org/roam/captured.org" "Notes")
           "* " :prepend t)
          ("C" "Roam capture note (HERE)" entry
           (file+headline buffer-file-name "TODOs")
           "* ")
          ("t" "Project TODO" entry
           (file+headline my/current-project-TODOs-file "TODOs")
           #'my/org-template/project-todo-capture)))
  (defun bespoke/org-clock-out-switch-to-state (state)
    (message "State: %s" state)
    (if (string= state "EVNT") "PAST" state))
  (setq org-clock-out-switch-to-state #'bespoke/org-clock-out-switch-to-state)

  ;; NOTE to avoid stupid functions for setting values of custom settings, find a good way of setting them here
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
  (setq orb-templates
    '(("r" "ref" plain
       (function org-roam-capture--get-point)
       ""
       :file-name "${citekey}"
       :head "#+title: ${title}\n#+roam_key: ${ref}\n#+roam_tags: @zasób @pracka\n"
       :unnarrowed t))
    )

  (setq attempt "ROAM_TAGS={@zasób}")
  (org-add-agenda-custom-command '("z" "Rzeczy uchwycone" tags-todo attempt))
  (org-add-agenda-custom-command '("x" "Dziennik" agenda "" ((org-agenda-start-with-log-mode t))))


  ;; Add an action to Helms listing buffers
  ;; (setf (alist-get "Remove from perspective" helm-type-buffer-actions nil t #'string=) nil)
  (eval-after-load 'helm
    '(nconc helm-type-buffer-actions
            '(("Remove from perspective" . (lambda (_) (persp-remove-buffer (helm-marked-candidates)))))))

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
 '(aw-scope 'frame)
 '(evil-want-Y-yank-to-eol nil)
 '(evil-want-change-word-to-end nil)
 '(package-selected-packages
   '(lsp-latex company-reftex company-auctex auctex-latexmk general smooth-scroll ox-gfm ox-hugo ob-async csv-mode insert-shebang flycheck-bashate fish-mode company-shell edit-server yequake proof-general company-coq company-math math-symbol-lists merlin-eldoc stickyfunc-enhance helm-gtags helm-cscope xcscope ggtags counsel-gtags counsel swiper org-pdftools org-noter-pdftools org-noter org-roam-bibtex dap-mode posframe bui sbt-mode tide typescript-mode kotlin-mode flycheck-kotlin tern org-roam pdf-tools org-journal origami yapfify yaml-mode xterm-color vterm utop tuareg caml terminal-here shell-pop seeing-is-believing rvm ruby-tools ruby-test-mode ruby-refactor ruby-hash-syntax rubocopfmt rubocop rspec-mode robe rjsx-mode rbenv rake pytest pyenv-mode py-isort pippel pipenv pyvenv pip-requirements ocp-indent ob-elixir mvn multi-term minitest meghanada maven-test-mode lsp-python-ms lsp-java live-py-mode importmagic epc ctable concurrent deferred helm-pydoc groovy-mode groovy-imports pcache gradle-mode git-gutter-fringe+ fringe-helper git-gutter+ flycheck-ocaml merlin flycheck-mix flycheck-credo eshell-z eshell-prompt-extras esh-help emojify emoji-cheat-sheet-plus dune cython-mode company-emoji company-anaconda chruby bundler inf-ruby browse-at-remote blacken auto-complete-rst anaconda-mode pythonic alchemist elixir-mode smeargle orgit magit-gitflow magit-popup helm-gitignore gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link evil-magit git-commit with-editor web-mode tagedit slim-mode scss-mode sass-mode pug-mode less-css-mode helm-css-scss haml-mode emmet-mode ox-reveal web-beautify livid-mode skewer-mode simple-httpd json-mode json-snatcher json-reformat js2-refactor yasnippet multiple-cursors js2-mode js-doc coffee-mode scala-mode mmm-mode markdown-toc markdown-mode gh-md org-projectile org-category-capture org-present org-pomodoro alert log4e gntp org-mime org-download lv htmlize gnuplot racket-mode faceup slime-company company slime ws-butler winum which-key volatile-highlights vi-tilde-fringe uuidgen use-package toc-org spaceline powerline restart-emacs request rainbow-delimiters popwin persp-mode pcre2el paradox spinner org-plus-contrib org-bullets open-junk-file neotree move-text macrostep lorem-ipsum linum-relative link-hint indent-guide hydra hungry-delete hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation helm-themes helm-swoop helm-projectile helm-mode-manager helm-make projectile pkg-info epl helm-flx helm-descbinds helm-ag google-translate golden-ratio flx-ido flx fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist highlight evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-args evil-anzu anzu evil goto-chg undo-tree eval-sexp-fu elisp-slime-nav dumb-jump f dash s diminish define-word column-enforce-mode clean-aindent-mode bind-map bind-key auto-highlight-symbol auto-compile packed aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line helm avy helm-core popup async))
 '(safe-local-variable-values
   '((org-roam-db-directory . "~/.cache/org-roam/org-roam-dotty-wiki.db")
     (org-roam-directory . "~/workspace/dotty-wiki")
     (org-roam-db-directory "~/.cache/org-roam/org-roam-dotty-wiki.db")
     (org-roam-directory "~/workspace/dotty-wiki")
     (javascript-backend . tide)
     (javascript-backend . tern)
     (javascript-backend . lsp)))
 '(winum-scope 'frame-local))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
)
