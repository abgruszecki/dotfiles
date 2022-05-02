# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
	. "$HOME/.bashrc"
    fi
fi

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/.local/bin" ] ; then
    PATH="$HOME/.local/bin:$PATH"
fi

if [ -e /home/gruszecki/.nix-profile/etc/profile.d/nix.sh ]; then . /home/gruszecki/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer

PATH=~/.local/bin:$PATH
PATH=~/.cargo/bin:$PATH
PATH=$PATH:~/pkg/git-fuzzy/bin
PATH=$PATH:/home/gruszecki/.local/share/coursier/bin
PATH=$PATH:~/opt/coursier/bin
PATH=$PATH:~/opt/metals-emacs/bin

export PATH

export PAGER=less

__PROFILE_SOURCED=yes

# opam configuration
if test -r ~/.opam/opam-init/init.sh 
then source ~/.opam/opam-init/init.sh > /dev/null 2> /dev/null
else true
fi
