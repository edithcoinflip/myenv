#!/bin/bash
 # .bashrc

# User specific aliases and functions

# Source global definitions
if [ -f /etc/bash.bashrc ]; then
    . /etc/bash.bashrc
fi

if [ -f /etc/bashrc ]; then
    . /etc/bashrc
fi

#one time per session
if [ -z "$MYRC_PREPATH" ]
then
    export MYRC_PREPATH=$PATH
    export MYLD_PATH=$LD_LIBRARY_PATH
fi
export PATH=${MYRC_PREPATH}
export LD_LIBRARY_PATH=${MYLD_PATH}:/usr/local/jump/mpfr/lib

#######
#Exports
########
export EDITOR=emacs
export HISTCONTROL=ignoreboth
export HISTFILESIZE=25000
export HISTSIZE=25000
export COMP_TAR_INTERNAL_PATHS=1

#######
#Shell Opts
###########
set -o ignoreeof
shopt -s histappend
shopt -s cdspell
shopt -s checkwinsize

# stop here if non interactive (scp for example)
[ -z "$PS1" ] && return

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

##docker setup
eval "$(direnv hook bash)"

[[ $- == *i* ]] && stty -ixon

#########
# Aliases
#########
if [ -f ~/.bash_alias ]; then
    source ~/.bash_alias
fi

###########
# Functions
###########
if [ -f ~/.bash_func ]; then
    source ~/.bash_func
fi


#############
# Completions
#############

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).

MYBASHCOMP="/etc/bash_completion"
case $- in
    *i*) [[ -f ${MYBASHCOMP} ]] && source ${MYBASHCOMP} ;;
esac
unset MYBASHCOMP

MYBASHCOMP="/usr/share/bash-completion/bash_completion"
case $- in
    *i*) [[ -f ${MYBASHCOMP} ]] && source ${MYBASHCOMP} ;;
esac
unset MYBASHCOMP

MYBASHCOMP="/usr/local/etc/profile.d/bash_completion.sh"
case $- in
    *i*) [[ -f ${MYBASHCOMP} ]] && source ${MYBASHCOMP} ;;
esac
unset MYBASHCOMP

########
# PROMPT
########
if [ -f ~/.bash_prompt ]; then
    source ~/.bash_prompt
fi

export ATTRIBUTES=
export TERM=xterm-256color
export MAKEFLAGS=-j8
export PATH
export LD_LIBRARY_PATH

if [ -z "$PYTHONPATH" ]; then
    export PYTHONPATH=${EXTRA_PYTHONPATH}
else
    export PYTHONPATH+=:${EXTRA_PYTHONPATH}
fi

#export PYTHONSTARTUP=~/.pythonrc

export TOOLCHAIN="c11"

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
