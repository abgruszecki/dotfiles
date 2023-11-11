;;; $DOOMDIR/+embark.el -*- lexical-binding: t; -*-

(map! :map embark-url-map
      "C-'" #'org-web-tools-insert-link-for-url)

(setf (alist-get #'org-web-tools-insert-link-for-url embark-pre-action-hooks)
      `(embark--mark-target ~embark//delete-region)
      )
