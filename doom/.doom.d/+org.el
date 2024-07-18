;;; $DOOMDIR/+org.el -*- lexical-binding: t; -*-

(use-package! org-roam-bibtex
  :after org-roam
  :config (progn
            (add-hook! org-mode #'org-roam-bibtex-mode)))

(use-package! org-web-tools
  :after org)

(setq! org-todo-keywords '((sequence "TODO(t)" "DONE(d)")
                           (sequence "STEP(s)" "DONE(d)")
                           (sequence "TASK(k)" "DONE(d)")
                           (sequence "OPEN(o)" "CLSD(c)")
                           (sequence "EVNT(e)" "PAST(p)"))
       org-reverse-note-order t
       ;; org-agenda-files "~/.cache/emacs-org-mode/agenda"
       org-refile-targets '((org-agenda-files . (:level . 1)))
       org-outline-path-complete-in-steps nil
       org-refile-use-outline-path t
       org-export-with-toc nil
       org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id
       org-hide-block-startup t
       org-startup-folded "fold")

(setq! org-capture-templates
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
         ;; ("e1" "Dotty weekly item" entry
         ;;  (file+olp "~/org/roam/captured.org" "Wydarzenia" "Dotty weekly")
         ;;  "*** %?")
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

(setq! bibtex-completion-bibliography '("~/.cache/zotero-export/PhD.bib")
       bibtex-completion-library-path "~/zotero-pdf/"
       bibtex-completion-notes-path "~/org/roam/"
       bibtex-completion-pdf-open-function (lambda ($1) (call-process "open" nil 0 nil $1))
       org-ref-show-citation-on-enter nil)

(setq! citar-bibliography '("~/.cache/zotero-export/PhD.bib"))

(setq! org-roam-directory "~/org/roam"
       org-roam-db-location "~/.cache/org-roam/org-roam.db"
       org-roam-node-display-template "${title:120} ${tags:30}")

(setq! org-roam-capture-templates
          `(("z" "zasób" plain "%?"
             :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                                "#+title: ${title}\n#+filetags: :@zasób:\n")
             :unnarrowed t)

            ("l" "log")
            ("ll" "log/doktorat" entry "* %^{Verb|Read|Seen|Re-read}: [[%(bspk//capture-register :url)][%(bspk//capture-register :title)]]  %(org-set-tags \":log:\")%?"
             :if-new (file+olp "~/org/roam/doktorat.org" ("Notes"))
             :prepend t
             :prepare-finalize ,(lambda ()
                                  (beginning-of-buffer)
                                  (org-align-tags))
             )

            ("p" "pracka" plain "%?"
             :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                                "#+title: ${title}\n#+filetags: :@pracka:@zasób:\n\n* ZTK\n* Wiedza")
             :unnarrowed t)

            ("s" "standalone" plain "%?"
             :if-new (file+head "standalone/%<%Y%m%d%H%M%S>-${slug}.org"
                                "#+title: ${title}\n#+filetags: :@standalone:\n")
             :unnarrowed t)

            ("d" "domena" plain "%?"
             :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                                "#+title: ${title}\n#+filetags: :@domena:\n")
             :unnarrowed
             )))

;; These are already set by Doom.
;; (map! :mode org-mode
;;       :localleader
;;       :prefix "M-r"
;;       "f" #'org-roam-node-find
;;       "i" #'org-roam-node-insert
;;       "l" #'org-roam-buffer-toggle
;;       "c" #'org-roam-buffer-capture
;;       "a" 'org-roam-tag-add
;;       "r" 'org-roam-tag-remove
;;       "a" 'org-roam-alias-add
;;       )

(defun ~org-ref-cite-insert-helm ()
  (interactive)
  ;; TODO Load org-roam-bibtex-mode more elegantly?
  (org-roam-bibtex-mode 1)
  (org-ref-cite-insert-helm))

(defun ~org-roam-visit-attachment-dir ()
  (interactive)
  (let ((dir (file-name-concat "~/org/attachments"
                               (file-name-with-extension
                                (file-name-sans-extension
                                 (file-name-nondirectory (buffer-file-name)))
                                ".d"))))
    ;; (when ())
    (when (file-regular-p dir)
      (error "Path unexpectedly points to a regular file: %s" dir)
        )
    (make-directory dir t)
    (dired dir)))

(map! :leader :prefix ("\\" . "Private keybindings")
 :n "b" #'~org-ref-cite-insert-helm)
