set -x EDITOR vim

# TODO move aliases to a separate file?
alias g git

alias l ls
alias ll "ls -l"

alias lz eza
alias lzl "eza -l"

# TODO what's tere?
function tere
    set --local result (~/.cargo/bin/tere $argv)
    [ -n "$result" ] && cd -- "$result"
end

# TODO move this to my notes?
# function fish_right_prompt
#     if test $CMD_DURATION
#         # Show duration of the last command in seconds
#         set duration (echo "$CMD_DURATION 1000" | awk '{printf "%.3fs", $1 / $2}')
#         echo $duration
#     end
# end

set -g fish_greeting "Welcome to fish, the friendly interactive shell. Remember: M-e for navi, M-E for aichat."
 
bind alt-e _navi-smart-replace
bind alt-E _aichat_fish
bind alt-left      backward-token
bind alt-right     forward-token
bind alt-backspace backward-kill-token
bind ctrl-alt-left      backward-bigword
bind ctrl-alt-right     forward-bigword
bind ctrl-alt-backspace backward-kill-bigword

if command -q fzf
    fzf --fish | source
end

set -gx PATH $PATH ~/.local/bin
set -gx PATH $PATH ~/.config/emacs/bin
set -gx PATH $PATH ~/.bun/bin
set -gx PATH $PATH ~/opt/juliaup/bin

set -gx MANPATH $MANPATH ~/.local/share/man

### FISHER
# See fisher readme reg. `fisher_path`.

set -gx fisher_path ~/.config/fisher

set fish_function_path $fish_function_path[1] $fisher_path/functions $fish_function_path[2..-1]
set fish_complete_path $fish_complete_path[1] $fisher_path/completions $fish_complete_path[2..-1]

for file in $fisher_path/conf.d/*.fish
    source $file
end

### EXTERNAL CONFIGURATION

#### VTERM

function vterm_printf;
    if [ -n "$TMUX" ]
        # tell tmux to pass the escape sequences through
        # (Source: http://permalink.gmane.org/gmane.comp.terminal-emulators.tmux.user/1324)
        printf "\ePtmux;\e\e]%s\007\e\\" "$argv"
    else if string match -q -- "screen*" "$TERM"
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$argv"
    else
        printf "\e]%s\e\\" "$argv"
    end
end

if [ "$INSIDE_EMACS" = 'vterm' ]
    function clear
        vterm_printf "51;Evterm-clear-scrollback";
        tput clear;
    end
end

if [ "$INSIDE_EMACS" = 'vterm' ]
    function vterm_prompt_end;
        vterm_printf '51;A'(whoami)'@'(hostname)':'(pwd)
    end
    functions --erase vterm_old_fish_prompt
    functions --copy fish_prompt vterm_old_fish_prompt
    function fish_prompt --description 'Write out the prompt; do not replace this. Instead, put this at end of your file.'
        # Remove the trailing newline from the original prompt. This is done
        # using the string builtin from fish, but to make sure any escape codes
        # are correctly interpreted, use %b for printf.
        printf "%b" (string join "\n" (vterm_old_fish_prompt))
        vterm_prompt_end
    end
end

#### mise
# NOTE I could also try out doing `mise en` on shell start and otherwise calling it manually?
# set -x MISE_PARANOID 1
~/.local/bin/mise activate fish | source

#### aider
set -x AIDER_ENV_FILE $HOME/.ai/.env

#### coursier
set PATH $PATH ~/.local/share/coursier/bin

# source ~/anaconda3/etc/fish/conf.d/conda.fish
# set -q GHCUP_INSTALL_BASE_PREFIX[1]; or set GHCUP_INSTALL_BASE_PREFIX $HOME ; set -gx PATH $HOME/.cabal/bin $PATH /home/gruszecki/.ghcup/bin # ghcup-env

# The next line updates PATH for the Google Cloud SDK.
# if [ -f '~/Downloads/google-cloud-cli-linux-x86_64/google-cloud-sdk/path.fish.inc' ];
#     source '~/Downloads/google-cloud-cli-linux-x86_64/google-cloud-sdk/path.fish.inc'
# end
