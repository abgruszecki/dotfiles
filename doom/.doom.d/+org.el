;;; $DOOMDIR/+org.el -*- lexical-binding: t; -*-

(setq! org-todo-keywords '((sequence "TODO(t)" "DONE(d)")
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
       org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id
       org-hide-block-startup t
       org-startup-folded "fold")

(setq! org-roam-node-display-template "${title:120} ${tags:30}")

(setq! bibtex-completion-bibliography '("~/.cache/zotero-export/PhD.bib")
       bibtex-completion-library-path "~/zotero-pdf/"
       bibtex-completion-notes-path "~/org/roam/"
       bibtex-completion-pdf-open-function (lambda ($1) (call-process "open" nil 0 nil $1))
       org-ref-show-citation-on-enter nil)

(map! :mode org-mode
      :localleader
      :prefix "M-r"
      "f" #'org-roam-node-find
      "i" #'org-roam-node-insert
      "l" #'org-roam-buffer-toggle
      "c" #'org-roam-buffer-capture
      "a" 'org-roam-tag-add
      "r" 'org-roam-tag-remove
      "a" 'org-roam-alias-add
      )
