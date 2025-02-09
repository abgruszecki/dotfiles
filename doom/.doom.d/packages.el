;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;(unpin! pinned-package)
;; ...or multiple packages
;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;(unpin! t)

;; (package! evil-surround)
;; (package! evil-snipe :disable t)
;; (package! evil-textobj-anyblock :disable t)

;; (disable-packages! evil-textobj-anyblock)

;; NOTE Seems: an unlisted dependency of a listed package won't be correct updated.
;; At least: I had to manually add parsebib here b/c it wasn't updating as helm-bibtex expected.

(package! bsp-utils :recipe (:local-repo "+bsp/utils"))
(package! evil-lisp-state)

;; (package! emacsql)
;; (package! org-roam)
(package! parsebib :pin "c0ee4d5f10bf801af03f633b6b73ced4a0ffead7")
(package! helm-bibtex :pin "6064e8625b2958f34d6d40312903a85c173b5261")
(package! org-ref)
(package! org-roam-bibtex)

(package! org-web-tools)
(package! ox-gfm)

;; (package! gptel)
(package! aider :recipe (:host github :repo "tninja/aider.el" :files ("*.el")))

(package! typst-ts-mode
  :recipe (:type git :host sourcehut :repo "meow_king/typst-ts-mode")
  :pin "6a35d230")

;; Claude gave me some hints here: https://console.anthropic.com/workbench/b89a4c24-845c-4c05-8c78-c36aa3c0c9cb .
(package! bookmark+
  ;; This is a mirror, actual files are hosted on the Emacs Wiki.
  ;; See https://www.emacswiki.org/emacs/BookmarkPlus
  :recipe (:type git :host github :repo "emacsmirror/bookmark-plus")
  :pin "e077023")
