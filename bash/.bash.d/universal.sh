# .bash_universal : Bash bindings for any host

__cmd_exists() {
    command -v "$1" >/dev/null 2>&1
}

export EDITOR=vim

PS1='\u@\h:\W\$ '

### (Only apply if current shell is interactive)
if [[ $- = *i* ]]; then

shopt -s histappend
# Some programmable completions, like for tmux, are broken.
# complete -r tmux doesn't seem to fix the problem.
# I think many progcomps are useful though, like git's.
# This would remove *all* progcomps.
# complete -r
# This would disable custom completion instead.
# shopt -u progcomp

HISTCONTROL=ignoreboth

HISTSIZE=200000
HISTFILESIZE=1000000

# Every time a command is ran, add it to the history
# (To read them use `history -n` or just start a new session.)
PROMPT_COMMAND="history -a; $PROMPT_COMMAND"

# Readline bindings
# NOTE: use `bind -p` to figure out existing bindings

# See https://stackoverflow.com/questions/38960716/prevent-accidental-history-editing-in-bash 
bind 'set revert-all-at-newline on'          # revert all history edits after nl
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

__cmd_exists fzf && eval "$(fzf --bash)"
__cmd_exists zoxide && eval "$(zoxide init bash)"

fi
