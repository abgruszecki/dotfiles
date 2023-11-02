(defun bsp/make-lb ()
  (cons nil nil))

(defun bsp/lb-append (lb elt)
  (setf (cdr lb) (nconc (cdr lb) (list elt)))
  (unless (car lb)
    (setf (car lb) (cdr lb)))
  (setf (cdr lb) (last (cdr lb))))
