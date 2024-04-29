;;; $DOOMDIR/autoload/workspace.el -*- lexical-binding: t; -*-

;;;###autoload
(defun ~workspace/become ()
  (interactive)
  ;; TODO: what to do if current workspace is non-empty?
  ;; Do I even need to do anything at all if I use the DOOM API?
  ;; (+workspace/save t)
  (+workspaces-delete-associated-workspace-h)
  (call-interactively #'+workspace/load))
