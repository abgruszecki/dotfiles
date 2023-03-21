set EDITOR vim
export EDITOR

alias g git

alias annihilate "rm -r"

alias l ls
alias ll "ls -l"

function mkcd -a dir
  mkdir $dir
  or return
  cd $dir
end

function up -a dir
    if [ -z "$dir" ]
        return
    end
    set target (string replace -r "/\Q$dir\E/.*" "/$dir" $PWD)
    echo $target
    cd $target
end

complete -x -k -c up -a '(string split -n / $PWD)'

function tere
    set --local result (~/.cargo/bin/tere $argv)
    [ -n "$result" ] && cd -- "$result"
end

### VTERM

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

function fish_right_prompt
    if test $CMD_DURATION
        # Show duration of the last command in seconds
        set duration (echo "$CMD_DURATION 1000" | awk '{printf "%.3fs", $1 / $2}')
        echo $duration
    end
end

source ~/anaconda3/etc/fish/conf.d/conda.fish

set -q GHCUP_INSTALL_BASE_PREFIX[1]; or set GHCUP_INSTALL_BASE_PREFIX $HOME ; set -gx PATH $HOME/.cabal/bin $PATH /home/gruszecki/.ghcup/bin # ghcup-env