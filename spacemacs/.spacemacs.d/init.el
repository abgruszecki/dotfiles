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
   ;; Paths must have a trailing slash (i.e. "~/.mycontribs/")
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
     ;; -------------------------------------------------------------------------

     ;; This layer seems like it uses a package (`evil-mc') with a lot of issues.
     ;; Creating new cursors erases all registers (including macros).
     ;; Some commands apparently might be incompatible.
     ;; iedit seems to do the same job better?
     ;; But the freedom to set multiple cursors anywhere might be worth it.
     ;; multiple-cursors

     helm
     (org :variables
          org-enable-github-support t
          org-enable-reveal-js-support t
          org-enable-org-journal-support t
          org-enable-roam-support t)
     eww
     bibtex
     pdf

     (lsp :variables
          lsp-lens-enable nil
          lsp-ui-sideline-enable nil)
     (tree-sitter :variables
                  tree-sitter-syntax-highlight-enable nil)
     git
     (shell :variables
            shell-default-position 'bottom)

     ;; agda

     shell-scripts
     emacs-lisp
     common-lisp
     clojure
     coq
     haskell
     (python :variables python-backend 'anaconda)
     javascript
     racket
     raku
     kotlin

     ipython-notebook

     csv
     yaml

     (latex :packages (not company-math)
            :variables
            latex-backend 'company-auctex
            latex-enable-auto-fill nil)
     markdown
     html

     (scala :variables
            scala-backend 'scala-metals
            scala-auto-treeview nil)

     ;; bespoke!
     corsair
     dotty
     bespoke-scala-mode
     bespoke-org-roam
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
		 (dollar :location local)
     chatgpt
     eros
     lister
     org-noter
     org-noter-pdftools
     org-ql ov peg transient ts ;; org-ql and dependencies
		 org-super-agenda
     persist
     sqlite3
     general
     yequake
     )

   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()

   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages
   '(org-superstar
     evil-escape
     auto-highlight-symbol
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
   ;; If non-nil then enable support for the portable dumper. You'll need to
   ;; compile Emacs 27 from source following the instructions in file
   ;; EXPERIMENTAL.org at to root of the git repository.
   ;;
   ;; WARNING: pdumper does not work with Native Compilation, so it's disabled
   ;; regardless of the following setting when native compilation is in effect.
   ;;
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

   ;; Scale factor controls the scaling (size) of the startup banner. Default
   ;; value is `auto' for scaling the logo automatically to fit all buffer
   ;; contents, to a maximum of the full image height and a minimum of 3 line
   ;; heights. If set to a number (int or float) it is used as a constant
   ;; scaling factor for the default logo size.
   dotspacemacs-startup-banner-scale 'auto

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

   ;; If non-nil, show file icons for entries and headings on Spacemacs home buffer.
   ;; This has no effect in terminal or if "all-the-icons" package or the font
   ;; is not installed. (default nil)
   dotspacemacs-startup-buffer-show-icons nil

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
   dotspacemacs-default-font '("FantasqueSansMono Nerd Font"
                               :size 12.0
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
   ;; (default t) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup t

   ;; If non-nil the frame is undecorated when Emacs starts up. Combine this
   ;; variable with `dotspacemacs-maximized-at-startup' to obtain fullscreen
   ;; without external boxes. Also disables the internal border. (default nil)
   dotspacemacs-undecorated-at-startup nil

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90

   ;; A value from the range (0..100), in increasing opacity, which describes the
   ;; transparency level of a frame background when it's active or selected. Transparency
   ;; can be toggled through `toggle-background-transparency'. (default 90)
   dotspacemacs-background-transparency 90

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
   ;; but only visual lines are counted. For example, folded lines will not be
   ;; counted and wrapped lines are counted as multiple lines.
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

   ;; Color highlight trailing whitespace in all prog-mode and text-mode derived
   ;; modes such as c++-mode, python-mode, emacs-lisp, html-mode, rst-mode etc.
   ;; (default t)
   dotspacemacs-show-trailing-whitespace t

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup 'changed

   ;; If non-nil activate `clean-aindent-mode' which tries to correct
   ;; virtual indentation of simple modes. This can interfere with mode specific
   ;; indent handling like has been reported for `go-mode'.
   ;; If it does deactivate it here.
   ;; (default t)
   dotspacemacs-use-clean-aindent-mode t

   ;; Accept SPC as y for prompts if non-nil. (default nil)
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
   ;; and todos. If non-nil only the file name is shown.
   dotspacemacs-home-shorten-agenda-source nil

   ;; If non-nil then byte-compile some of Spacemacs files.
   dotspacemacs-byte-compile nil))

(defun dotspacemacs/user-env ()
  "Environment variables setup.
This function defines the environment variables for your Emacs session. By
default it calls `spacemacs/load-spacemacs-env' which loads the environment
variables declared in `~/.spacemacs.env' or `~/.spacemacs.d/.spacemacs.env'.
See the header of this file for more information."
  (spacemacs/load-spacemacs-env)
)

(defun dotspacemacs/user-init ()
  "Initialization for user code:
This function is called immediately after `dotspacemacs/init', before layer
configuration.
It is mostly for variables that should be set before packages are loaded.
If you are unsure, try setting them in `dotspacemacs/user-config' first."

  ;; Should I just require some packages here, so that they are always available?
  ;; (require 'dollar)

  (add-to-list 'package-archives '("jcs-elpa" . "https://jcs-emacs.github.io/jcs-elpa/packages/") t)
  (add-to-list 'package-archive-priorities '("jcs-elpa" . 0) t)

  ;; Part of the fix for https://github.com/Somelauw/evil-org-mode/issues/93
  (fset 'evil-redirect-digit-argument 'ignore) ;; before evil-org loaded

  ;; Part of the fix for https://github.com/Somelauw/evil-org-mode/issues/93
  (spacemacs|use-package-add-hook evil-org-mode
    :post-config
    (add-to-list 'evil-digit-bound-motions 'evil-org-beginning-of-line)
    (evil-define-key 'motion 'evil-org-mode
      (kbd "0") 'evil-org-beginning-of-line)

    ;; These don't unbind the key properly.
    ;; The bindings are preceded by the mode.
    (define-key evil-org-mode-map (kbd "C-<return>") nil)
    (define-key evil-org-mode-map (kbd "C-S-<return>") nil)
    ;; This does properly unbind the keys.
    (evil-define-key '(normal insert) evil-org-mode-map
      (kbd "C-<return>") #'nil
      (kbd "C-M-<return>") #'nil)

    (evil-define-key '(normal insert) org-mode-map
      (kbd "C-<return>") #'org-meta-return
      (kbd "C-M-<return>") #'evil-org-org-insert-heading-respect-content-below)
    )

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

  (spacemacs|use-package-add-hook undo-tree
    :post-config
    (setf undo-tree-auto-save-history nil)
    (push 'coq-mode undo-tree-incompatible-major-modes)
    )

  (spacemacs|use-package-add-hook tex
    :post-config
    (evil-declare-motion #'LaTeX-find-matching-begin)
    (evil-declare-motion #'LaTeX-find-matching-end)
    )

  ;; (setq debug-on-message ".*libgccjit\\.so.*")
  ;; (setq byte-compile-debug t)
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

  (use-package eros
    :demand t
    :config
    (eros-mode 1))
  (use-package persist
    :config
    (setq persist--directory-location "~/.spacemacs.d/persist"))
  (use-package lister)
  (use-package lister-mode)

  ;; sometimes the smartparens self-insert hook inexplicably doesn't get inserted
  ;; manually using the package fixes that
  (use-package smartparens
    :config
    (progn
      ;; disable pairing ' and ` in minibuffer (most often I'm entering Lisp there)
      (sp-local-pair '(minibuffer-mode minibuffer-inactive-mode) "'" nil :actions nil)
      (sp-local-pair '(minibuffer-mode minibuffer-inactive-mode) "`" nil :actions nil)

      (sp-with-modes '(tex-mode plain-tex-mode latex-mode LaTeX-mode)
        (sp-local-pair "$" nil :actions nil)
        )
      ))

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

  (use-package general)

  ;; configure `compile' to stop first error
  (setq compilation-scroll-output 'first-error)

  ;; NOTE: this does not seem necessary,
  ;; since Emacs automatically recompiles .el files as they are saved
  ;; (byte-recompile-directory "~/.spacemacs.d/" 0)
  (load "~/.spacemacs.d/bespoke")
  (load "~/.spacemacs.d/bespoke-yequake")
  (load "~/.spacemacs.d/bespoke-perspective")
  (load "~/.spacemacs.d/bespoke-org-mode")
  (load "~/.spacemacs.d/bespoke-org-ql")
  (load "~/.spacemacs.d/bespoke-dotty")
  (load "~/.spacemacs.d/bespoke-projects")
  (load "~/.spacemacs.d/bespoke-latex-mode")

  (setq org-roam-v2-ack t)
  (org-roam-db-autosync-enable)

  ;;; configuration

  (load-file (let ((coding-system-for-read 'utf-8))
               (shell-command-to-string "agda-mode locate")))

  ;; (setq TeX-view-program-list '(("Evince" "evince --page-index=%(outpage) %o")))
  (spacemacs|use-package-add-hook auctex
    :post-config
    (setf (alist-get 'output-pdf TeX-view-program-selection) '("PDF Tools"))
    )

  (progn ;; kludges
    )

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
            (prog1 (apply yank-func args)
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
                #'my-spacemacs//yank-ident-region)

    ;; redefine `spacemacs/zoom-frm-in'
    ;; add support for setting the zoom degree with the prefix argument
    (defun spacemacs/zoom-frm-in (prefix)
      "zoom in frame, but keep the same pixel size"
      (interactive "p")
      (let ((frame-zoom-font-difference
             (if (> prefix 0) prefix frame-zoom-font-difference)))
        (spacemacs//zoom-frm-do 1)
        (spacemacs//zoom-frm-powerline-reset)))

    )

  (progn ;; ace-window
    (defun bsp/ace-window-always ()
      "Like `ace-window', but always selects a window."
      (interactive)
      (let ((aw-dispatch-always t))
        (call-interactively #'ace-window)))
    (spacemacs/set-leader-keys "wW" #'bsp/ace-window-always)
    )

  (progn ;; compile
    (setq compilation-skip-threshold 2) ;; skip over everything but errors
    )

  (progn ;; dired
    (setq dired-auto-revert-buffer t))

  (progn ;; popwin
    (setf (plist-get (alist-get 'compilation-mode popwin:special-display-config) :height)
          0.15)
    )

  (progn ;; chatgpt
    (setq openai-key (with-temp-buffer (insert-file-contents "~/.openai")
                                       (s-trim (buffer-string)))))

  (progn ;; evil
    (setq evilnc-comment-text-object ";")
    (define-key evil-inner-text-objects-map evilnc-comment-text-object #'evilnc-inner-commenter)
    (define-key evil-outer-text-objects-map evilnc-comment-text-object #'evilnc-outer-commenter)

    (evil-define-key 'normal 'global
      (kbd "M-;") #'evilnc-comment-operator)

    (evil-define-key 'visual 'global
      "x" (lambda () (interactive)
            (let ((expand-region-contract-fast-key "X"))
              (er/expand-region 1)))
      )
    )

  (progn ;; evil-tex
    (dolist (fn '(evil-tex-go-back-section
                  evil-tex-go-forward-section
                  LaTeX-find-matching-begin
                  LaTeX-find-matching-end
                  ))
      (evil-set-command-properties fn :jump t))

    (evil-define-key 'motion LaTeX-mode-map
      (kbd "[ M-[") #'LaTeX-find-matching-begin
      (kbd "] M-]") #'LaTeX-find-matching-end)
    )

  (progn ;; proof-general
    (defun my-pg//make-undo-great-again ()
      (setq-local evil-undo-function #'pg-protected-undo))
    (add-hook 'coq-mode-hook #'my-pg//make-undo-great-again)

    (spacemacs/set-leader-keys-for-major-mode 'coq-mode
      "r" #'proof-retract-current-goal
      )
    )

  (progn ;; shell
    (setq shell-default-shell 'vterm)
    )

  (progn ;; lsp
    (setq lsp-signature-auto-activate nil
          lsp-prefer-flymake nil

          ;; lsp-ui
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

  (progn ;; latex
    (cl-pushnew 'latex-mode spacemacs-indent-sensitive-modes)
    (defun bespoke/inhibit-local-electric-indent ()
      (setq-local electric-indent-inhibit t))
    (add-hook 'LaTeX-mode-hook #'bespoke/inhibit-local-electric-indent)
    (remove-hook 'LaTeX-mode-hook #'LaTeX-math-mode)

    ;; TODO: move only to a line that starts with \begin or \end
    ;; TODO: allow jumping to the next occurence (i.e. drop the current matching pair, find the next one)
    ;; TODO: jump to the other occurence being edited if it's off-screen, then come back to origin
    (defun bsp/wip-evil-iedit-latex-env ()
      (interactive)
      (evil-iedit-state/iedit-mode 1)
      (save-excursion
        (if (s-contains-p "\\begin" (thing-at-point 'line))
            (iedit-expand-down-to-occurrence)
          (iedit-expand-up-to-occurrence))))

    (general-define-key
     :keymaps 'LaTeX-mode-map
     "H-'" #'bsp/wip-evil-iedit-latex-env)
    )

  (progn ;; lsp-latex
    (setq lsp-latex-build-executable "latexrun"
          lsp-latex-build-args '("%f")
          lsp-latex-build-on-save t
          lsp-latex-build-aux-directory "latex.out")
    )

  (defun bespoke/disable-latex-checkers ()
    (setq flycheck-disabled-checkers (cl-list* 'tex-chktex 'tex-lacheck flycheck-disabled-checkers)))
  (progn
    (add-hook 'LaTeX-mode-hook #'bespoke/disable-latex-checkers))

  (progn ;; auctex
    (setq TeX-auto-local ".auctex.d")
    )

  (progn ;; flycheck
    (setq flycheck-check-syntax-automatically '(save mode-enabled)))

  ;;; fun

  (defun my-sbt/abort ()
    (interactive)
    (with-current-buffer "*sbt<dotty>*"
      (comint-interrupt-subjob)))

  ;;; keybindings

  (defun bespoke-hack/test ()
    (interactive)
    (push (cons 'no-record ?\s) unread-command-events))

  (define-key key-translation-map
    (kbd "M-,") (kbd dotspacemacs-major-mode-emacs-leader-key))

  ;; (define-key key-translation-map
  ;;   (kbd "H-SPC") (kbd dotspacemacs-emacs-leader-key))

  ;; (general-define-key
  ;;  :prefix "H-SPC"
  ;;  "SPC" #'bespoke-hack/test
  ;;  "TAB" #'eyebrowse-last-window-config
  ;;  "ESC" #'yequake-retoggle
  ;;  "<f1>" #'my-eyebrowse/toggle-magit)

  ;; (general-define-key
  ;;  "")

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
   :prefix "H-<f9>"
   "l" #'my-lsp)

  (general-define-key
   :prefix "H-\\" ;; IDEA: the "hyper insert" key
   "<tab>" #'helm-company)

  (general-define-key
   :keymaps 'emacs-lisp-mode-map
   "H-." #'lisp-state-toggle-lisp-state)

  (evil-define-key 'motion 'global
    (kbd "<C-tab>") nil)

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
    ;; (kbd "H-,") (kbd "ESC SPC m")
    )

  (spacemacs/set-leader-keys
    "b H-a" #'bespoke/annex-shared-buffers
    )

  (spacemacs/set-leader-keys "o#" #'sbt/console)
  (spacemacs/set-leader-keys
    "gd" #'my/magit/kill-all-buffers
    "rh" #'my/help-resume
    "oc" #'my-sbt/abort
    "oa" #'my-super-agenda/go
    "ob" #'bespoke-org-ref/top-helm-bibtex
    "o C-l" #'my-lsp)

  (spacemacs/set-leader-keys-for-major-mode 'lisp-mode
    "," #'lisp-state-toggle-lisp-state)

  ;; Add an action to Helms listing buffers
  ;; (setf (alist-get "Remove from perspective" helm-type-buffer-actions nil t #'string=) nil)
  (eval-after-load 'helm
    '(nconc helm-type-buffer-actions
            '(("Remove from perspective" . (lambda (_) (persp-remove-buffer (helm-marked-candidates))))))))

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
   '(dogears eros chatgpt cdlatex org-ql lister persist helm-cider clojure-snippets cider-eval-sexp-fu cider sesman parseedn clojure-mode parseclj raku-mode flycheck-raku tree-sitter-langs tree-sitter tsc lsp-latex company-reftex company-auctex auctex-latexmk general smooth-scroll ox-gfm ox-hugo ob-async csv-mode insert-shebang flycheck-bashate fish-mode company-shell edit-server yequake proof-general company-coq company-math math-symbol-lists merlin-eldoc stickyfunc-enhance helm-gtags helm-cscope xcscope ggtags counsel-gtags counsel swiper org-pdftools org-noter-pdftools org-noter org-roam-bibtex dap-mode posframe bui sbt-mode tide typescript-mode kotlin-mode flycheck-kotlin tern org-roam pdf-tools org-journal origami yapfify yaml-mode xterm-color vterm utop tuareg caml terminal-here shell-pop seeing-is-believing rvm ruby-tools ruby-test-mode ruby-refactor ruby-hash-syntax rubocopfmt rubocop rspec-mode robe rjsx-mode rbenv rake pytest pyenv-mode py-isort pippel pipenv pyvenv pip-requirements ocp-indent ob-elixir mvn multi-term minitest meghanada maven-test-mode lsp-python-ms lsp-java live-py-mode importmagic epc ctable concurrent deferred helm-pydoc groovy-mode groovy-imports pcache gradle-mode git-gutter-fringe+ fringe-helper git-gutter+ flycheck-ocaml merlin flycheck-mix flycheck-credo eshell-z eshell-prompt-extras esh-help emojify emoji-cheat-sheet-plus dune cython-mode company-emoji company-anaconda chruby bundler inf-ruby browse-at-remote blacken auto-complete-rst anaconda-mode pythonic alchemist elixir-mode smeargle orgit magit-gitflow magit-popup helm-gitignore gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link evil-magit git-commit with-editor web-mode tagedit slim-mode scss-mode sass-mode pug-mode less-css-mode helm-css-scss haml-mode emmet-mode ox-reveal web-beautify livid-mode skewer-mode simple-httpd json-mode json-snatcher json-reformat js2-refactor yasnippet multiple-cursors js2-mode js-doc coffee-mode scala-mode mmm-mode markdown-toc markdown-mode gh-md org-projectile org-category-capture org-present org-pomodoro alert log4e gntp org-mime org-download lv htmlize gnuplot racket-mode faceup slime-company company slime ws-butler winum which-key volatile-highlights vi-tilde-fringe uuidgen use-package toc-org spaceline powerline restart-emacs request rainbow-delimiters popwin persp-mode pcre2el paradox spinner org-plus-contrib org-bullets open-junk-file neotree move-text macrostep lorem-ipsum linum-relative link-hint indent-guide hydra hungry-delete hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation helm-themes helm-swoop helm-projectile helm-mode-manager helm-make projectile pkg-info epl helm-flx helm-descbinds helm-ag google-translate golden-ratio flx-ido flx fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist highlight evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-args evil-anzu anzu evil goto-chg undo-tree eval-sexp-fu elisp-slime-nav dumb-jump f dash s diminish define-word column-enforce-mode clean-aindent-mode bind-map bind-key auto-highlight-symbol auto-compile packed aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line helm avy helm-core popup async))
 '(safe-local-variable-values
   '((bespoke/project-class quote dotty)
     (eval bespoke/set-dotty-project-vars)
     (org-roam-db-directory . "~/.cache/org-roam/org-roam-dotty-wiki.db")
     (org-roam-directory . "~/workspace/dotty-wiki")
     (org-roam-db-directory "~/.cache/org-roam/org-roam-dotty-wiki.db")
     (org-roam-directory "~/workspace/dotty-wiki")
     (javascript-backend . tide)
     (javascript-backend . tern)
     (javascript-backend . lsp)))
 '(warning-suppress-types '((comp)))
 '(winum-scope 'frame-local))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(highlight-parentheses-highlight ((nil (:weight ultra-bold))) t))
)
