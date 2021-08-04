;; -*- mode: emacs-lisp; lexical-binding: t -*-

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
            )))

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
                       stamp)))))

(defun bespoke/org-clock-out-switch-to-state (state)
  (message "State: %s" state)
  (if (string= state "EVNT") "PAST" state))
(setq org-clock-out-switch-to-state #'bespoke/org-clock-out-switch-to-state)

(spacemacs/set-leader-keys
  "oje" #'my-org/jump-to-events
  "oja" #'my-org-roam/jump-to-agenda-file
  )

(spacemacs/set-leader-keys-for-major-mode 'org-mode
  "i <f2>" #'org-roam-insert
  "i `" #'my-org/insert-code-block
  "oa" #'org-archive-to-archive-sibling
  "o'" #'my-org/change-event-timestamps ; TODO - change to DWIM
  "oi" #'my/org/set-ztk-id
  "os" #'my-org/sort-todos
  "oc" #'my-org/set-created
  "o1" #'my-org/duplicate-repeating-meeting
  "o2" #'my-org/archive-repeating-meeting
  "C-'" #'dotty/edit-scala3-trace)

(spacemacs/set-leader-keys-for-minor-mode 'org-capture-mode
  "C-," #'bespoke-org/capture-finalize-and-jump)
