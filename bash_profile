#!/usr/bin/bash
#export PATH=/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:~/bin:

export LANG=C

export XDG_CONFIG_HOME=~/.config

export BSH_VERSION=0.6.1-SNAPSHOT

set-java9() {
	export JAVA_HOME=$(/usr/libexec/java_home -v9)
}

set-java8() {
	export JAVA_HOME=$(/usr/libexec/java_home -v1.8)
}

set-java7() {
	export JAVA_HOME=$(/usr/libexec/java_home -v1.7)
}

set-java8

alias edit-bash_profile="vim ~/.bash_profile"
alias edit-vimrc="vim ~/.vimrc"

alias annihilate="rm -rf"

export CLICOLOR=1
export EDITOR=vim

# superawesome prompt
export PS1=$'\\[\e[37;44m\\] $ \\[\e[01;30m\\]\ue0b1\ue0b1 \\[\e[37m\\]\W \\[\e[0m\\]\\[\e[34;40m\\]\ue0b0\\[\e[0m\\] '
# \\[\e[37;44m\\]\u@\h
# \\[\e[01;30;44m\\]\ue0b0
# \\[\e[00;33;44m\\]\ue0b0

alias ls='gls --color=auto'
eval "$(gdircolors -b ~/.config/dircolors)"

alias l='gls -l --color=auto'
alias ll='gls -l --color=auto'
alias lll='gls -lA --color=auto'
alias la='gls -A --color=auto'

alias ags='ag -S'

alias g='git'

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

for file in /usr/local/etc/bash_completion.d/*
do source "$file"
done
__git_complete g __git_main

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

loc() {
	dir=${2:-"."}
	find "$dir" -iname "*$1*"
}
