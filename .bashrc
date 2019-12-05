eval "$(direnv hook bash)"

[[ $- == *i* ]] && stty -ixon

[[ -r "/usr/local/etc/profile.d/bash_completion.sh" ]] && . "/usr/local/etc/profile.d/bash_completion.sh"

GITPROMPTFILE="/usr/local/opt/bash-git-prompt/share/gitprompt.sh"
if [ -f ${GITPROMPTFILE} ]; then
    __GIT_PROMPT_DIR="/usr/local/opt/bash-git-prompt/share"
    #GIT_PROMPT_ONLY_IN_REPO=1
    GIT_PROMPT_THEME=Solarized
    source ${GITPROMPTFILE}
fi
unset GITPROMPTFILE

if [ -f "${HOME}/.bash_alias" ]; then
    source "${HOME}/.bash_alias"
fi

if [ -f "${HOME}/.bash_func" ]; then
    source "${HOME}/.bash_func"
fi

