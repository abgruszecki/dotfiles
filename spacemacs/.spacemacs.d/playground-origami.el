;; -*- mode: emacs-lisp; lexical-binding: t -*-
(require 'origami)
(require 'origami-parsers)

(defun bsp-origami//simple-parser (regex start-marker end-marker)
  "Create a parser for simple start and end markers."
  (lambda (create)
    (lambda (content)
      (let ((positions (origami-get-positions content regex)))
        (origami-build-pair-tree create start-marker end-marker positions)))))

(defun bsp-origami//dotty-trace-parser (create)
  ;; TODO it seems that it is easiest to work with logs
  ;; if "details" appear one line below the trace headers:
  ;; ==> ?
  ;; {{{
  ;;   x = 1
  ;; }}}
  ;; <== a
  ;; `arrows' fold too much, they shouldn't fold the headers.
  ;; writing a custom parser should fix that.
  ;; can be copied from `origami-lisp-parser'.
  (let ((arrows (funcall (origami-markers-parser "==>" "<==") create))
        (braces (funcall (origami-markers-parser "{{{" "}}}") create)))
    (lambda (content)
      (origami-fold-children (origami-fold-shallow-merge
                              (origami-fold-root-node (funcall arrows content))
                              (origami-fold-root-node (funcall braces content)) ))))
  )

(setf (alist-get 'dotty-trace-playground-mode origami-parser-alist) #'bsp-origami//dotty-trace-parser)

(define-derived-mode dotty-trace-playground-mode fundamental-mode "Dotty trace"
  (origami-mode)

  (font-lock-add-keywords
   nil
   `(,(concat "^ *" (regexp-opt (list "<==" "==>"))) . 'font-lock-keyword-face))
  (font-lock-mode t)
  )
