;;; $DOOMDIR/+embark.el -*- lexical-binding: t; -*-

(defun ~embark-open-terminal (dir)
  "Open an external termirnal in a particular DIR."
  (interactive "sOpen externally in terminal: ")

  (unless (string-match-p "\\`[a-z]+://" dir)
    (setq dir (expand-file-name dir)))
  (if (not (file-exists-p dir))
      (message "Directory does not exist: `%s'" dir)
    (when (not (file-directory-p dir))
      (setf dir (file-name-directory dir))))

  (message "Opening `%s' in an external terminal..." dir)
  ;; Works on Ubuntu, maybe Debian too.
  (let ((default-directory dir))
    (call-process "x-terminal-emulator" nil 0 nil))
  )

(setq! embark-cycle-key "C-.")

(defvar-keymap embark-org-link-bespoke-map
  :doc "Bespoke org-link actions."
  :parent nil
  "r" #'~org-roam-relink-orb-to-citar)

(fset 'embark-org-link-bespoke-map embark-org-link-bespoke-map)

(map! :n "C-." nil
      "C-SPC" nil
      "C-." #'embark-act
      "C-," #'embark-dwim
      (:map embark-general-map
       "C-SPC" #'embark-select
       "C-S-SPC" #'mark ;; why would this be bound to C-SPC by default???
       ))

(after! embark
  (map! :map embark-url-map
        "C-'" #'org-web-tools-insert-link-for-url)

  (map! :map embark-file-map
        "C-x" #'~embark-open-terminal)

  (map! :map embark-org-link-map
        :desc "bespoke"
        "\\" 'embark-org-link-bespoke-map)

  (setf
   (alist-get #'org-web-tools-insert-link-for-url embark-pre-action-hooks)
   `(embark--mark-target ~embark//delete-region)
   )
  )

(defun ~citar-copy-short-title (citekey)
  (let* ((entry (citar-get-entry citekey))
         (res (or (alist-get "shorttitle" entry nil nil #'equal)
                  (alist-get "title" entry nil nil #'equal))))
    (kill-new res)
    ;; (message "Copied as kill: %s" res)
    ))

(after! (embark citar)
  (map! :map citar-citation-map
        :desc "Copy short title as kill"
        "w" #'~citar-copy-short-title
        )
  )
