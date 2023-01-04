(require 'org-ql)
(require 'org-ql-view)
(require 'magit-section)
(require 'ts)

(defun bsp-org/buffer-list ()
  (->> (buffer-list)
       (-filter ($ (eq 'org-mode (buffer-local-value 'major-mode $1))))))

(defun bsp-org/buffer-title (buf)
  (with-current-buffer buf
    (let ((kw (org-collect-keywords (list "TITLE") (list "TITLE"))))
      (alist-get "TITLE" kw nil nil #'equal))))

(defun bsp-org-ql/agenda-item-marker (item)
  (or (get-text-property 0 'org-marker item)
      (get-text-property 0 'org-hd-marker item))
  (error "")
  )

(defun bsp-org-ql/agenda-item-buffer-title (item)
  (->> (bsp-org-ql/agenda-item-marker item)
       (marker-buffer)
       (bsp-org/buffer-title)))

(defun bsp-org-ql/created< (a b)
  (org-time< (org-element-property :CREATED a)
             (org-element-property :CREATED b)))

(defun bsp-org-ql/created> (a b)
  (not (bsp-org-ql/created< a b)))

(defclass bsp-org-ql/log-day (magit-section) ())

;; This shows log entries from all currently opened org-mode buffers,
;; grouped by org-roam title, sorted by created date.
;; Unfortunately this is not what I want, since the grouping is before the sorting.
;; NOTE: it seems that super-groups have their own ordering?
;;;###autoload
(defun bsp-org-ql/list-logs ()
  (interactive)
  ;; NOTE: it seems that super-groups have their own ordering? compare:
  ;; (org-ql-search (bsp-org/buffer-list) '(local-tags "log")
  ;;   :super-groups '((:auto-property "CREATED"))
  ;;   :sort #'bsp-org-ql/created<)
  ;; (org-ql-search (bsp-org/buffer-list) '(local-tags "log")
  ;;   :super-groups '((:auto-property "CREATED"))
  ;;   :sort #'bsp-org-ql/created>)

  ;; (org-ql-search (bsp-org/buffer-list) '(local-tags "log")
  ;;   ;; :super-groups '((:auto-map bsp-org-ql/agenda-item-buffer-title))
  ;;   ;; :super-groups '((:auto-property "CREATED"))
  ;;   :sort #'bsp-org-ql/created>
  ;;   )

  (with-current-buffer (get-buffer-create "*foo*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (magit-section-mode)
      (let ((log-elts (org-ql-select (bsp-org/buffer-list) '(local-tags "log")
                        :action 'element-with-markers
                        :sort #'bsp-org-ql/created>)))

        (magit-insert-section (bsp-org-ql)
          (magit-insert-heading "Hello world!")
          (while log-elts
            ;; (insert (org-ql-view--format-element (pop log-elts)) "\n")

            (let* ((cur-date (org-element-property :CREATED (car log-elts)))
                   (same-date-log-elts (-take-while ($ (equal (org-element-property :CREATED $1)
                                                              cur-date))
                                                    log-elts)))
              (setf log-elts (-drop-while ($ (equal (org-element-property :CREATED $1)
                                                    cur-date))
                                          log-elts))
              ;; TODO magit sections are sensitive to where they end,
              ;; so maybe if the terminating newline of each section
              ;; wasn't a part of it, magit would deal better with detecting
              ;; current section?
              (magit-insert-section (bsp-org-ql/roam-node)
                (let ((fmt-cur-date (if cur-date
                                        (ts-format "%a, %d %B '%y" (ts-parse-org cur-date))
                                      "-")))
                  (magit-insert-heading fmt-cur-date))
                (dolist (elt same-date-log-elts)
                  (insert (org-ql-view--format-element elt) "\n")))
              )
            )
          ))))
  (display-buffer "*foo*")
  )
