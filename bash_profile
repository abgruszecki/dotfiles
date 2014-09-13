export PATH=/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:~/bin:
export JAVA_HOME=/Library/Java/JavaVirtualMachines/jdk1.7.0_67.jdk/Contents/Home
export CLICOLOR=1

# superawesome prompt

export PS1=$'\\[\e[37;44m\\] \u@\h \\[\e[01;30m\\]\ue0b1\ue0b1 \\[\e[37m\\]\W \\[\e[0m\\]\\[\e[34;40m\\]\ue0b0\\[\e[0m\\] '
# \\[\e[37;44m\\]\u@\h
# \\[\e[01;30;44m\\]\ue0b0
# \\[\e[00;33;44m\\]\ue0b0

alias ls='gls --color=auto'
eval `gdircolors -b ~/.dircolors`

alias ll='gls -lA --color=auto'
alias la='gls -A --color=auto'

mkcd() { mkdir $1; cd $1; }

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
