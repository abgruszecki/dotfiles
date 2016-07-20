export PATH=/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:~/bin:

export JAVA_8_HOME=$(/usr/libexec/java_home -v1.8)
export JAVA_7_HOME=$(/usr/libexec/java_home -v1.7)

alias set-java8="export JAVA_HOME=$JAVA_8_HOME"
alias set-java7="export JAVA_HOME=$JAVA_7_HOME"

set-java7

export CLICOLOR=1
export EDITOR=vim

# superawesome prompt
export PS1=$'\\[\e[37;44m\\] \u@\h \\[\e[01;30m\\]\ue0b1\ue0b1 \\[\e[37m\\]\W \\[\e[0m\\]\\[\e[34;40m\\]\ue0b0\\[\e[0m\\] '
# \\[\e[37;44m\\]\u@\h
# \\[\e[01;30;44m\\]\ue0b0
# \\[\e[00;33;44m\\]\ue0b0

alias ls='gls --color=auto'
eval $(gdircolors -b ~/.dircolors)

alias ll='gls -lA --color=auto'
alias la='gls -A --color=auto'

alias ags='ag -S'

alias g=git

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

for file in /usr/local/etc/bash_completion.d/*; do
	if [[ $file = *docker ]]; then continue; fi
	source "$file"
done

mvn-master-artifact() {
	mvn "$@" -Pmaster-artifact
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
