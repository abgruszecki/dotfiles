# .bash_universal : Bash bindings for any host

export EDITOR=vim

### (Only apply if current shell is interactive)
if [[ $- = *i* ]]; then
# Every time a command is ran, add it to the history
# (To read them use `history -n` or just start a new session.)
PROMPT_COMMAND="history -a; $PROMPT_COMMAND"

# Readline bindings
# NOTE: use `bind -p` to figure out existing bindings
bind 'set skip-completed-text on'            # after comp, skip common suffix
bind 'set completion-ignore-case on'         # comp: ignore case
bind 'set completion-map-case on'            # comp: conflate -/_
bind 'set menu-complete-display-prefix on'   # comp: display common prefix
bind 'set colored-completion-prefix on'      # comp: color common prefix
bind 'set show-all-if-ambiguous on'          # comp: show options immediately

# bad idea: unix-word-rubout is less incremental
# bind '"\e\C-?": unix-word-rubout'            # make M-Bckspc like C-w
bind '"\C-e": unix-word-rubout'              # make C-e like C-w (for Jupyter)
bind 'TAB:menu-complete'                     # comp: "menu" style (cycle)
bind '"\e[Z": menu-complete-backward'        # comp: S-TAB cycles backwards

bind '"\e[1;3D": shell-backward-word'        # C-Left
bind '"\e[1;3C": shell-forward-word'         # C-Right
bind '"\C-H": backward-kill-word'            # C-Bckspc (by default nothing)
bind '"\e\C-?": shell-backward-kill-word'    # M-Bckspc

command >/dev/null 2>&1 -v fzf && eval "$(fzf --bash)"
fi
