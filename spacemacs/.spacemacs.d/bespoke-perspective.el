;; -*- mode: emacs-lisp; lexical-binding: t; -*-

(setq persp-autokill-buffer-on-remove 'kill
      persp-kill-foreign-buffer-behaviour 'kill
      persp-auto-save-persps-to-their-file-before-kill 'persp-file)

(add-to-list 'persp-filter-save-buffers-functions
              #'(lambda (b)
                  (with-current-buffer b
                    (derived-mode-p 'magit-mode))))

(defvar //loaded-all nil)
(defun //load-all ()
  (unless //loaded-all
    (persp-load-state-from-file "bespoke")
    (persp-load-state-from-file "para")
    (persp-load-state-from-file "dotty")
    (setq //loaded-all t)))

(defun /bespoke-switcher ()
  (let ((standalone-bespokes
         (->>
          (directory-files dotspacemacs-directory t ".*\.el$")
          (--filter (not (equal (file-name-nondirectory it) "init.el")))
          (--map (cons (file-name-nondirectory it) it))
          ))
        (package-bespokes
         (->> (directory-files spacemacs-private-directory t)
              (--filter (and (file-directory-p it)
                             (not (-contains-p
                                   '("." ".." ".git"
                                     "local" "snippets" "templates")
                                   (file-name-nondirectory it)))))
              (--map (cons (file-name-nondirectory it) (concat it "/packages.el"))))))

    (helm :sources `(,(helm-build-sync-source "Spacemacs config"
                         :candidates (list (concat dotspacemacs-directory "init.el"))
                         :action #'find-file)
                     ,(helm-build-sync-source "Standalone files"
                        :candidates standalone-bespokes
                        :action #'find-file)
                     ,(helm-build-sync-source "Packages"
                        :candidates package-bespokes
                        :action #'find-file)))))

(defvar //bespoke-persp "bespoke")
(defun /switch-to-bespoke (&optional prefix interactive-p)
  (interactive "P\np")
  (require 'helm)
  (unless (persp-with-name-exists-p //bespoke-persp)
    (persp-load-state-from-file //bespoke-persp))
  (if (eq major-mode 'help-mode)
      (progn
        (pupo/close-window)
        (persp-switch //bespoke-persp)
        (my/help-resume))
    (if (and interactive-p (not prefix)
             (equal //bespoke-persp (spacemacs//current-layout-name)))
        (/bespoke-switcher)
      (persp-switch //bespoke-persp)
      (if (numberp prefix)
          (eyebrowse-switch-to-window-config prefix)
        (eyebrowse-switch-to-window-config-1)))))

(defvar //para-persp "para")
(defun /switch-to-para (&optional prefix interactive-p)
  (interactive "P\np")
  ;; (when (consp prefix)
  ;;   (setf frame-title-format "Dotty roam"
  ;;         //para-persp "dotty-roam"
  ;;         org-roam-directory "~/workspace/dotty-wiki"
  ;;         org-roam-db-directory "~/.cache/org-roam/org-roam-dotty-wiki.db"
  ;;         ))
  ;; (org-roam-mode 1)
  (unless (persp-with-name-exists-p //para-persp)
    (persp-load-state-from-file //para-persp))
  (if (and interactive-p
           (not prefix)
           (string= //para-persp (spacemacs//current-layout-name)))
      (call-interactively #'org-roam-node-find)
    (persp-switch //para-persp)
    (if (numberp prefix)
        (eyebrowse-switch-to-window-config prefix)
      (eyebrowse-switch-to-window-config-1)
      )
    ))

(defun /switch-to-dotty ()
  (interactive)
  (let ((p-name "dotty"))
    (unless (persp-with-name-exists-p p-name)
      (persp-load-state-from-file p-name))
    (persp-switch p-name)))

(defvar //current-dynamic-bindings nil)
(persist-defvar
 /known-dynamic
 '( "papers" "capture-calc" "eff-coeff" "superf" "idot"
    "scala3doc"
    "fos" "fos-coq" "parprog"
    "wynajem"
    )
 "Known dynamic perspectives."
 )

(defvar-local bespoke-listemode/local-variable nil
  "Variable being edited in current lister buffer.")

(defun bespoke-lister/delete-item-at-point ()
  (interactive '() 'lister-mode)
  (lister-delete-at lister-local-ewoc :point))

(defun bespoke-lister/save-list ()
  (interactive '() 'bespoke-lister-mode)
  (set (symbol-value 'bespoke-lister-mode/local-variable) (lister-get-list lister-local-ewoc)))

(defvar bespoke-lister-mode-map (make-sparse-keymap)
  "Keymap for bespoke lister-mode.")

(general-define-key
 :keymaps 'bespoke-lister-mode-map
 [remap evil-delete] #'bespoke-lister/delete-item-at-point
 [remap save-buffer] #'bespoke-lister/save-list)

(define-derived-mode bespoke-lister-mode fundamental-mode "lister/bspk"
  "Bespoke version of lister-mode."
  (lister-mode))

(defun bespoke/edit-known-dynamic ()
  "Interactively edit known dynamic perspectives."
  (interactive)
  (with-current-buffer (get-buffer-create "Bespoke IEdit: Known dynamic perspectives")
    (bespoke-lister-mode)
    (lister-setup (current-buffer) #'list)
    (lister-set-list lister-local-ewoc /known-dynamic)
    (setq bespoke-lister/local-variable '/known-dynamic)
    (display-buffer-same-window (current-buffer) nil)
    ))

(defun /switch-to-dynamic (force-pick &optional force-key)
  (interactive "P")
  (require 'helm) ;; apparently this helps with correctly loading Helm ?!?
  (let* ((k (or force-key
                (elt (this-command-keys-vector) 0)))
         (entry (alist-get k //current-dynamic-bindings)))
    (when (or force-pick (not entry))
      (-if-let (picked (helm :sources
                             `(,(helm-build-sync-source "Perspective name"
                                  :candidates /known-dynamic)
                               ,(helm-build-dummy-source "Create perspective"
                                  :action
                                  '(("Create new perspective" .
                                     //create-persp-with-home-buffer)
                                    ("Create new perspective with buffers from current project" .
                                     //create-persp-with-current-project-buffers)
                                    ("Create new perspective with buffers from current perspective" .
                                     //persp-copy))
                                  ))
                             ))
          (setf (alist-get k //current-dynamic-bindings) picked
                entry picked)
        (user-error "No perspective picked."))
      )
    (unless (persp-with-name-exists-p entry)
      (persp-load-state-from-file entry))
    (persp-switch entry)))

(defun //create-persp-with-home-buffer (name)
  (add-to-list '/known-dynamic name nil #'string=)
  (spacemacs//create-persp-with-home-buffer name)
  name)

(defun //create-persp-with-current-project-buffers (name)
  (add-to-list '/known-dynamic name nil #'string=)
  (spacemacs//create-persp-with-current-project-buffers name)
  name)

(defun //persp-copy (name)
  (add-to-list '/known-dynamic name nil #'string=)
  (persp-copy name)
  name)

(defun bespoke/trace-persp-add (oldfun buf persp)
  (with-current-buffer (get-buffer-create "*persp-trace*")
    (insert (format "[%s] Adding %s\n" (persp-name persp) buf)))
  (funcall oldfun buf persp))
(advice-add 'persp--buffer-in-persps-add :around #'bespoke/trace-persp-add)

(defun bespoke/trace-kill-buffer (oldfun &optional buf)
  (setq buf (or buf (current-buffer)))
  (funcall oldfun buf)
  (when (buffer-live-p buf)
    (let* ((persps (--map (persp-name it) (persp--buffer-in-persps buf)))
            (msg (format "Undead buffer: %s (in persps: %s)" buf persps)))
      (message msg)
      (read-string (concat "Hey chief! " msg))
      (with-current-buffer (get-buffer-create "*persp-trace*")
        (insert msg "\n")))))
(advice-add #'kill-buffer :around #'bespoke/trace-kill-buffer)

;; TODO: hierarchy.el offers a neat way of browsing hierarchies like this one
(defun bespoke/show-shared-buffers ()
  (interactive)
  (-let [bufs (->> (buffer-list)
                   (-map ($ (cons $1 (-map #'persp-name (persp--buffer-in-persps $1)))))
                   (-filter ($ (> (length (cdr $1)) 1))))]
    (with-current-buffer (get-buffer-create "*persp-multi-persp-buffers*")
      (erase-buffer)
      (insert (format "There are %s buffers in more than one perspective." (length bufs)))
      (cl-prettyprint bufs)
      (display-buffer-same-window (current-buffer) nil))))

(defun bespoke/annex-shared-buffers ()
  (interactive)
  (-let* ((bufs (->> (buffer-list)
                     (-map ($ (cons $1 (-map #'persp-name (persp--buffer-in-persps $1)))))
                     (-filter ($ (> (length (cdr $1)) 1)))))
         (selected (helm :sources (helm-build-sync-source "Buffers"
                                    :candidates (--map (cons (format "%-80s %s" (buffer-name (car it)) (cdr it))
                                                             (car it))
                                                       bufs)
                                    :action (lambda (marked)
                                              (setf marked (helm-marked-candidates))
                                              marked))
                         :buffer "*HELM Persp annex shared buffers*")))
    ;; (message "Selected following buffers: %s" selected)
    (cl-loop for buf in selected
             do (cl-loop for p in (persp--buffer-in-persps buf)
                         do (when (not (eq p (get-current-persp)))
                              (persp-remove-buffer buf p))))))

;; Local Variables:
;; read-symbol-shorthands: (("/" . "my-perspective/"))
;; End:
