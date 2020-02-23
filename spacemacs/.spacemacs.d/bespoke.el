
(defvar sbt/compile-arguments nil
  "Arguments passed to compile command in SBT")

;; -Xprint:all
;; -Xprompt
;; -uniqid
(setq sbt/compile-arguments "-color:never -d out")

(defvar-local sbt/output/source-buffer nil
  "Buffer that was used to create the output in this buffer")

(defvar-local sbt/output/command nil
  "Command that was used to create the output in this buffer")

(defvar-local sbt//inspect-input nil)

(defvar sbt/output/watched-buffers nil
  "Buffers with SBT output that are under watch")

(defvar sbt/last-operation nil
  "Last SBT command")

(defmacro sbt//invocation (command &rest arguments)
  "Used in interactive clause to both return arguments, and record that the command was invoked."
  `(-let [args (list ,@arguments)]
     (setq sbt/last-operation (cons ,command args))
     args)
  )

(defun sbt//ensure-console-open ()
  (projectile-ensure-project default-directory)
  (let* ((name (sbt/buffer-name))
         (buf (get-buffer-create name))
         (project-root (projectile-project-root)))
    (with-current-buffer buf
      (cd project-root)
      (make-comint-in-buffer name buf "sbt-cancelable.sh")
      (sbt/configure)
      buf)))

(defun sbt/buffer-name (&optional suffix)
  (concat "*sbt<" (projectile-project-name) ">" suffix "*"))

(defmacro sbt/in (&rest body)
  (declare (indent 0))
  `(with-current-buffer (sbt/buffer-name) ,@body))

(defun sbt/console ()
  (interactive)
  (select-window (display-buffer (sbt//ensure-console-open))))

(defun sbt/configure ()
  "Configure current SBT buffer"
  ;; NOTE despite docs, this is meaningful on its own (changes redirection behaviour)
  ;; NOTE comint-use-prompt-regexp is unset b/c it screws up input editing
  (setq comint-prompt-regexp "^sbt:[-a-zA-Z]+> ")

  (setq-local sbt//inspect-input t)

  (add-hook 'comint-redirect-hook 'sbt/post-redirect-cleanup nil t)
  )

;; TODO add a single buffer for locking

(defun sbt/lock ()
  "Lock last output buffer"
  (interactive)
  (let ((buffer-name (sbt/buffer-name "-output-locked")))
    (when (get-buffer buffer-name)
      (if (yes-or-no-p "Locked buffer already exists, delete it?")
          (kill-buffer buffer-name)
        (return))
      )

    (with-current-buffer (sbt/buffer-name "-output")
      (rename-buffer buffer-name)
      (sbt/watch)
      (display-buffer (current-buffer))
      )))

(defun sbt/locked ()
  "Display locked buffer"
  (interactive)
  (select-window (display-buffer (sbt/buffer-name "-output-locked"))))

(defun sbt/watch ()
  "Add current buffer to the list of watched buffers"
  (interactive)
  ;; TODO enable following once there is some form of queueing
  ;; (unless (member (current-buffer) sbt/output/watched-buffers)
  ;;   ;; TODO ensure that it is reasonable to have this buffer in the list (check if it has OK local vars?)
  ;;   (setq sbt/output/watched-buffers (cons (current-buffer) sbt/output/watched-buffers)))
  (setq sbt/output/watched-buffers (list (current-buffer))))

(defun sbt/output/refresh-watched ()
  "Refresh the output of watched buffers"
  (interactive (sbt//invocation
                'sbt/output/refresh-watched))
  (when sbt/output/watched-buffers
    ;; TODO watching multiple buffers is difficult, needs some sort of queueing
    (my/projectile/save-project-files)
    (with-current-buffer (car sbt/output/watched-buffers)
      (let ((source-buffer sbt/output/source-buffer)
            (source-cmd sbt/output/command))
        (with-current-buffer source-buffer
          (sbt/run source-cmd (car sbt/output/watched-buffers)))))))

(defun sbt/output/refresh (buffer)
  "Refresh the output of current buffer"
  (interactive (sbt//invocation
                'sbt/output/refresh
                (current-buffer)))
  (with-current-buffer buffer
    (my/projectile/save-project-files)
    (let ((source-buffer sbt/output/source-buffer)
          (source-cmd sbt/output/command)
          (target-buffer (current-buffer)))
      (with-current-buffer source-buffer
        (sbt/run source-cmd target-buffer)))))

(defun sbt/clean ()
  (ansi-color-filter-region (point-min) (point-max)))

(defun sbt/post-redirect-cleanup ()
  (unless (get-buffer comint-redirect-output-buffer)
    (error "Could not retrieve buffer for post-redirect cleanup (%s)" comint-redirect-output-buffer))

  (with-current-buffer comint-redirect-output-buffer
    ;; (sbt/clean)
    (origami-close-all-nodes (current-buffer))))

(defun sbt/run (cmd &optional target-buffer)
  (when (and target-buffer (get-buffer target-buffer))
    (with-current-buffer target-buffer
      (erase-buffer)))
  (let ((resolved-target-buffer (if target-buffer
                                    (get-buffer-create target-buffer)
                                  (generate-new-buffer "sbt-output"))))
    (with-current-buffer resolved-target-buffer
      (unless (eq major-mode 'dotty-trace-mode)
        (dotty-trace-mode))
      ;; TODO set these post-factum? keep track of command success?
      (setq sbt/output/source-buffer (sbt/buffer-name)
            sbt/output/command cmd))
    (sbt/in
      (comint-redirect-send-command cmd resolved-target-buffer nil))))

(defun sbt/compile-file (file)
  (interactive (sbt//invocation
                'sbt/compile-file
                (read-file-name "SBT compile file: ")))
  (my/projectile/save-project-files)
  ;; TODO escape the name of the file
  (sbt/run (concat "dotc " sbt/compile-arguments " " file)
           (sbt/buffer-name "-output")))

(defun sbt/compile-this-file ()
  (interactive)
  (my/projectile/save-project-files)
  (setq sbt/last-operation `(sbt/compile-file ,buffer-file-name))
  (sbt/compile-file buffer-file-name))

(defun sbt/send-command (command)
  (interactive (sbt//invocation
                'sbt/send-command
                (read-string "Command: ")))
  ;; TODO read through comint-redirect-send-command to see how it works
  (sbt/in
    (if (not (string-equal (buffer-substring (car comint-last-prompt)
                                             (cdr comint-last-prompt))
                           "sbt:dotty> "))
        (error "Process is not ready for input")
      (my/projectile/save-project-files)
      (delete-region (cdr comint-last-prompt)
                     (point-max))
      (goto-char (point-max))
      (insert command)
      (comint-send-input))
    (display-buffer (current-buffer))))

(defun sbt/send-test-command ()
  (interactive (sbt//invocation 'sbt/send-test-command))
  (sbt/send-command "testCompilation .eff."))

(defun sbt/send-compile-command ()
  (interactive (sbt//invocation 'sbt/send-compile-command))
  (sbt/send-command "compile"))

(defun sbt/repeat-last-operation ()
  (interactive)
  (unless sbt/last-operation
    (error "No SBT command was yet run."))
  (apply 'funcall (car sbt/last-operation) (cdr sbt/last-operation)))

(defun sbt/repeat-last-command ()
  "Repeats last command in SBT buffer"
  (interactive (sbt//invocation
                'sbt/repeat-last-command))
  (display-buffer (sbt//ensure-console-open))
  (my/projectile/save-project-files)
  (sbt/in
    (goto-char (point-max))
    (comint-previous-input 1)
    (comint-send-input)))

(defun sbt/comint-send-input-advice (underlying &rest args)
  (interactive)
  (unless (not (bound-and-true-p sbt//inspect-input))
    (my/projectile/save-project-files))
  (apply underlying args))

(advice-add 'comint-send-input :around 'sbt/comint-send-input-advice)

;;; Dotty trace mode

(define-derived-mode dotty-trace-mode fundamental-mode "Dotty trace"

  ;; font-lock
  (font-lock-add-keywords
   nil
   `(,(concat "^ *" (regexp-opt (list "<==" "==>"))) . 'font-lock-keyword-face))
  (font-lock-mode t)

  ;; origami
  (setq-local origami-fold-style 'triple-braces)
  (origami-mode 1)

  )

(evil-define-key 'normal dotty-trace-mode-map
  (kbd "RET") 'origami-forward-toggle-node
  (kbd "<tab>") (lambda () (interactive) (re-search-forward "^#"))
  (kbd "<backtab>") (lambda () (interactive) (re-search-backward "^#"))
  (kbd "<C-tab>") (lambda () (interactive) (re-search-forward "^#!"))
  (kbd "<C-iso-lefttab>") (lambda () (interactive) (re-search-backward "^#!"))
  "g%" 'dotty/jump-to-matching-trace-header
  )

;;; My functions

(defun my/current-project-root ()
  (car (project-roots (project-current))))

(defun my/current-project-TODOs-file ()
  (concat (my/current-project-root) "/TODOs.org"))

(defun my/org-template/project-todo-capture ()
  (with-temp-buffer
    (insert-file-contents "~/.spacemacs.d/org-templates/project-todo.org")
    (buffer-string)))

;; TODO ask to save all files in project when trying to run a command in SBT?

(defun dotty/current-line-trace-header ()
  (save-excursion
    (goto-char (line-beginning-position))
    (let ((result (or
                   (and (looking-at " *==>") 'opening)
                   (and (looking-at " *<==") 'closing))))
      (and result (cons result (- (match-end 0) (match-beginning 0) 3))))))

(evil-define-motion dotty/jump-to-matching-trace-header ()
  :type line
  (-if-let ((type . size) (dotty/current-line-trace-header))
      (ecase type
        ('opening
         (re-search-forward
          (rx-to-string `(seq bol (repeat ,size " ") "<=="))))

        ('closing
         (re-search-backward
          (rx-to-string `(seq bol (repeat ,size " ") "==>")))))
    (error "Not looking at a trace header!")))

(defun my/jump-to-indent (direction cmp)
  "Test doc"
  (interactive)
  (let ((start-indent (current-indentation)))
    ;; TODO pick up the first indent along the way if the current line is empty
    (while
        (and (not (bobp))
             (zerop
              ;; NOTE forward-line returns the number of lines it tried to
              ;; to move, but couldn't - 0 means "all good"
              (forward-line (or direction 1)))
             (or (looking-at-p "^$")
                 ;; TODO ignore comment lines?
                 (cond
                  ((= 0 cmp) (> (current-indentation) start-indent))
                  ((> 0 cmp) (<= (current-indentation) start-indent))
                  ((< 0 cmp ) (>= (current-indentation) start-indent)))))))
  (back-to-indentation))

(defmacro my/def-indent-variant (name direction cmp)
  `(evil-define-motion ,name ()
     :type line
     :jump t
     (interactive)
     (my/jump-to-indent ,direction ,cmp)))

(my/def-indent-variant my/forwards-jump-to-outdent       1  1)
(my/def-indent-variant my/forwards-jump-to-same-indent   1  0)
(my/def-indent-variant my/forwards-jump-to-indent        1 -1)
(my/def-indent-variant my/backwards-jump-to-outdent     -1  1)
(my/def-indent-variant my/backwards-jump-to-same-indent -1  0)
(my/def-indent-variant my/backwards-jump-to-indent      -1 -1)

(defun my/jump-to-lesser-indent (direction)
  (interactive "P")
  (let ((start-indent (current-indentation)))
    (while
        (and (not (bobp))
             (zerop (forward-line (or direction 1)))
             (or (= (current-indentation) 0) ; should instead check if line is empty
                 (>= (current-indentation) start-indent)))))
  (back-to-indentation))

(defun my/projectile/save-project-files ()
  "Save files in current project."
  (-let [project-root (projectile-project-root)]
    (save-some-buffers nil (lambda ()
                             (projectile-project-buffer-p (current-buffer)
                                                          project-root)))))

(evil-define-command my/slurp ()
  :repeat t
  ;; :suppress-operator t
  (interactive)
  (save-excursion
    (evil-up-paren ?{ ?} 1)
    (unless (or (move-text--at-last-line-p)
                (and (move-text--last-line-is-just-newline)
                     (move-text--at-penultimate-line-p)))
      (forward-line 1)
      (transpose-lines 1)
      (forward-line -2))
    (beginning-of-line)
    (unless (looking-at "\n")
      (indent-for-tab-command))))

(defun my/magit/kill-all-buffers ()
  (interactive)
  (mapc #'kill-buffer (magit-mode-get-buffers)))

(defvar dotty//prints-commented-out-p nil)
(defun dotty/toggle-printing ()
  (interactive)
  (save-excursion
    (if (not dotty//prints-commented-out-p)
        (mapc (lambda (buf) (with-current-buffer buf
                              (replace-string "trace.force(" "trace/*force*/(" nil (point-min) (point-max))
                              (replace-string "new Printer" "/*newPrinter*/noPrinter" nil (point-min) (point-max))))
              (projectile-project-buffers))
      (mapc (lambda (buf) (with-current-buffer buf
                            (replace-string "trace/*force*/(" "trace.force(" nil (point-min) (point-max))
                            (replace-string "/*newPrinter*/noPrinter" "new Printer" nil (point-min) (point-max))))
            (projectile-project-buffers))))
  (setf dotty//prints-commented-out-p (not dotty//prints-commented-out-p))
  (message "Dotty printing is: %s" (if dotty//prints-commented-out-p "disabled" "enabled")))
