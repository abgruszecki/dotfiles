;; -*- mode: emacs-lisp; lexical-binding: t -*-

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
(eval-after-load 'dotty #'my-dotty//configure)
