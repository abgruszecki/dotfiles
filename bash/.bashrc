# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
shopt -s globstar

# make less more friendly for non-text input files, see lesspipe(1)
[[ -x /usr/bin/lesspipe ]] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color|*-256color) color_prompt=yes;;
esac

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
#force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
	# We have color support; assume it's compliant with Ecma-48
	# (ISO/IEC-6429). (Lack of such support is extremely rare, and such
	# a case would tend to support setf rather than setaf.)
	color_prompt=yes
    else
	color_prompt=
    fi
fi

if [ "$color_prompt" = yes ]; then
    PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
else
    PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
fi
unset color_prompt force_color_prompt

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
    PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
    ;;
*)
    ;;
esac

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# colored GCC warnings and errors
#export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

# some more ls aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi

#END ubuntu

#BEGIN readline
bind 'set show-all-if-ambiguous on'
bind 'set show-all-if-unmodified on'
bind 'set menu-complete-display-prefix on'
bind '"\t": menu-complete'
bind '"\e[Z": menu-complete-backward'
#END readline

#export PATH=/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:~/bin:

# export LANG=C

export XDG_CONFIG_HOME=~/.config

# export BSH_VERSION=0.6.1-SNAPSHOT
#
# set-java9() {
# 	export JAVA_HOME=$(/usr/libexec/java_home -v9)
# }
#
# set-java8() {
# 	export JAVA_HOME=$(/usr/libexec/java_home -v1.8)
# }
#
# set-java7() {
# 	export JAVA_HOME=$(/usr/libexec/java_home -v1.7)
# }
#
# set-java8

alias edit-bash_profile="vim ~/.bash_profile"
alias edit-vimrc="vim ~/.vimrc"

alias annihilate="rm -rf"

export CLICOLOR=1
export PAGER=less
export EDITOR=vim

# superawesome prompt
export PS1=$'\\[\e[37;44m\\] $ \\[\e[01;30m\\]\ue0b1\ue0b1 \\[\e[37m\\]\W \\[\e[0m\\]\\[\e[34;40m\\]\ue0b0\\[\e[0m\\] '
# export PS1='\[\e[37;44m\] $ \[\e[01;30m\]\ue0b1\ue0b1 \[\e[37m\]\W \[\e[0m\]\[\e[34;40m\]\ue0b0\[\e[0m\] '
# \\[\e[37;44m\\]\u@\h
# \\[\e[01;30;44m\\]\ue0b0
# \\[\e[00;33;44m\\]\ue0b0

# alias ls='ls --color=auto'
eval "$(dircolors -b ~/.config/dircolors)"

# alias l='gls -l --color=auto'
# alias ll='gls -l --color=auto'
# alias lll='gls -lA --color=auto'
# alias la='gls -A --color=auto'

alias ags='ag -S'
alias g='git'
alias rg='ripgrep.rg'

# export NVM_DIR="$HOME/.nvm"
# source "$NVM_DIR/nvm.sh"

function git-dirstat {
	for file in *
	do
		ls -1 --color=always --directory "$file"
		git status --porcelain=v1 -- "$file" | sed $'s/^/\t/'
	done
}

function amm {
	command amm --no-remote-logging "$@"
}

function ozip {
	[[ -z $1 ]] && {
		echo "Usage: ozip <ZIPFILE>"
		return 1
	}
	outname=${1%.*}
	unzip "$1" -d "$outname"
}

function shasum-r {
	[[ -z $1 ]] && {
		echo "Usage: shasum-r <DIR>"
		return 1
	}
	(
		cd "$1" || exit
		find . -type f -exec shasum -a 256 {} \; | sort | shasum -a 256
	)
}

function rsync {
	command rsync --info=progress2 "$@"
}

mkcd() {
	mkdir -p "$1"
	cd "$1"
}

up() {
	if [ -z "$1" ]; then
		return
	fi
	local up=$1
	cd "${PWD/\/$up\/*//$up}"
}

_up() {
	local cur=${COMP_WORDS[COMP_CWORD]}
	local d=${PWD//\//\ }
	COMPREPLY=( $( compgen -W "$d" -- "$cur" ) )
}
complete -F _up up

# for file in /usr/local/etc/bash_completion.d/*
# do source "$file"
# done
# __git_complete g __git_main

bundle-mvn() {
	local gitbranch;
	if [[ $BUNDLE_WORKAREA == shared ]]
	then
		gitbranch=master
	elif [[ -n $BUNDLE_WORKAREA ]]
	then
		gitbranch="feature_$BUNDLE_WORKAREA"
	fi

	local -a props;

	if [[ -n $gitbranch ]]
	then props+=("-DfinalName.escapedGitBranch=$gitbranch")
	fi

	if [[ -n $BUNDLE_BRANCH ]]
	then props+=("-DfinalName.tsBranch=$BUNDLE_BRANCH")
	fi

	if [[ -n $BUNDLE_HOST ]]
	then props+=("-Dwildfly.hostname=$BUNDLE_HOST")
	fi

	mvn "${props[@]}" "$@"
}

set-bundle() {
	if [[ ! -d .bit-props ]]; then
		echo ./.bit-props not found!
		return 1
	fi
	local project=$(cat .bit-props/project-name)
	local branch=$(git rev-parse --abbrev-ref HEAD)
	local bundle="${project}_${branch//\//_}.war"

	local server=${1:-vagrantbox}
	if [[ $2 ]]; then
		local wa=$2
	else
		local wa=${branch##.*/}
	fi
	local text="<app><bundle>$bundle</bundle></app>"
	local file="/default/main/$project/WORKAREA/$wa/iwov-resources/ls-bundle.xml"

	ssh "$server" "echo \"$text\" > \"$file\""
}

pull-wa() {
	if [[ ! -d .bit-props ]]; then
		echo ./.bit-props not found!
		return 1
	fi
	local project=$(cat .bit-props/project-name)
	local branch=$(git rev-parse --abbrev-ref HEAD)
	local bundle="${project}_${branch//\//_}.war"

	local server=${1:-vagrantbox}
	if [[ $2 ]]; then
		local wa=$2
	else
		local wa=${branch##.*/}
	fi


}

[ -f "${XDG_CONFIG_HOME:-$HOME/.config}"/fzf/fzf.bash ] && source "${XDG_CONFIG_HOME:-$HOME/.config}"/fzf/fzf.bash

source ~/pkg/forgit/forgit.sh

export PATH=/home/gruszecki/.local/bin:$PATH
export PATH=$PATH:~/pkg/git-fuzzy/bin
export PATH="$PATH:/home/gruszecki/.local/share/coursier/bin"
export PATH=$PATH:~/opt/coursier/bin
export PATH=$PATH:~/opt/metals-emacs/bin
