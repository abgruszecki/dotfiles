;;; bsp-utils --- Summary
;;; Commentary:
;;; Code:
(defun make-lb ()
  "Make a list buffer."
  (cons nil nil))

(defun lb-append (lb elt)
  "Append ELT to the list buffer LB."
  (setf (cdr lb) (nconc (cdr lb) (list elt)))
  (unless (car lb)
    (setf (car lb) (cdr lb)))
  (setf (cdr lb) (last (cdr lb))))

(provide 'bsp-utils)
;;; bsp-utils.el ends here
