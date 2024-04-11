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

(map! :map embark-url-map
      "C-'" #'org-web-tools-insert-link-for-url)

(map! :map embark-file-map
      "C-x" #'~embark-open-terminal)

(setf (alist-get #'org-web-tools-insert-link-for-url embark-pre-action-hooks)
      `(embark--mark-target ~embark//delete-region)
      )
