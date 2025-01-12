;;; ../dotfiles/doom/.doom.d/+ai.el -*- lexical-binding: t; -*-

(require 's)

(use-package! gptel
  :config
  (setq! gptel-model 'claude-3-5-sonnet-20241022
         gptel-backend (gptel-make-anthropic
                        "Claude"
                        :stream t
                        :key #'~ai-load-anthropic-key)
         )
  )

(defun ~ai-load-anthropic-key ()
  (with-temp-buffer
    (insert-file-contents "~/.ai/ANTHROPIC_API_KEY")
    (s-trim (buffer-string))))

(defvar ~aider-base-args '("--no-git"
                           "--no-check-update"))

(defvar ~ai-model-groq-llama31-70b)
(defvar ~ai-model-claude35plus)
(setq ~ai-model-claude35plus "claude-3-5-sonnet-20241022")
(setq ~ai-model-groq-llama31-70b "llama-3.3-70b-versatile")

(use-package! aider
  :config
  (setq! aider-args (append
                     ;; "--model" "anthropic/claude-3-5-sonnet-20241022"
                     ;; "--model" "groq/llama3-70b-8192"
                     ;; '("--model" "groq/llama-3.1-70b-versatile")
                     `("--model" ,(concat "groq/" ~ai-model-groq-llama31-70b))
                     ~aider-base-args)
         )
  (setenv "ANTHROPIC_API_KEY" (~ai-load-anthropic-key))
  )

(defun aider-doom-setup-keys ()
  "Make `aider-doom-setup-keys' a noop.")

(defvar ~aider-buffer-name-override)

(fset #'~old-aider-buffer-name (symbol-function #'aider-buffer-name))

(defun aider-buffer-name ()
  (or ~aider-buffer-name-override
      (~old-aider-buffer-name)
      ))

(defun ~aider-run-aider-groq ()
  (interactive)
  (let ((aider-args (append `("--model" ,(concat "groq/"
                                                 ~ai-model-groq-llama31-70b))
                            ~aider-base-args))
        (~aider-buffer-name-override "*aider__groq_global*"))
    (aider-run-aider)
    ))

(defun ~aider-run-aider-sonnet ()
  (interactive)
  (let ((aider-args (append `("--chat-mode" "ask"
                              "--model" ,(concat "anthropic/"
                                                 ~ai-model-claude35plus))
                            ~aider-base-args))
        (~aider-buffer-name-override "*aider__ask_sonnet_global*"))
    (aider-run-aider)
    ))

(defun ~aider-doom-setup-keys ()
  "Setup Aider keybindings if the current buffer is in a git repository."
  (map! :leader
        (:prefix ("A" . "Aider")
                 (:prefix ("a" . "Add")
                  :desc "Current file" "c" #'aider-add-current-file
                  :desc "Files in window" "w" #'aider-add-files-in-current-window
                  :desc "Batch direct marked files" "b" #'aider-batch-add-dired-marked-files
                  :desc "Find files in repo" "g" #'aider-repo-find-name-dired
                  :desc "Open repo root" "d" #'aider-git-repo-root-dired)

                 (:prefix ("b" . "Buffer")
                  :desc "Switch to Aider" "b" #'aider-switch-to-buffer
                  :desc "Clear Aider" "c" #'aider-clear)

                 (:prefix ("s" . "Send")
                  :desc "File read-only" "f" #'aider-current-file-read-only
                  :desc "Line at cursor" "l" #'aider-send-line-under-cursor
                  :desc "Paragraph at cursor" "p" #'aider-send-paragraph)

                 (:prefix ("c" . "Code")
                  :desc "Change" "c" #'aider-code-change
                  :desc "Refactor region" "r" #'aider-region-refactor
                  :desc "Undo change" "u" #'aider-undo-last-change)

                 (:prefix ("d" . "Discuss")
                  :desc "Ask question" "a" #'aider-ask-question
                  :desc "Architecture" "d" #'aider-architect-discussion
                  :desc "Region explanation" "r" #'aider-region-explain
                  :desc "Exception debugggin" "e" #'aider-debug-exception)

                 (:prefix ("z" . "Other")
                  :desc "General command" "c" #'aider-general-command
                  :desc "Help" "h" #'aider-help
                  :desc "Show last commit" "g" #'aider-magit-show-last-commit)

                 :desc "Open Aider (with Sonnet)" "o" #'~aider-run-aider-sonnet
                 :desc "Open Aider (with Groq)" "O" #'~aider-run-aider-groq
                 :desc "Reset Aider" "r" #'aider-reset
                 :desc "Exit Aider" "x" #'aider-exit)))

(~aider-doom-setup-keys)
