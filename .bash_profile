#!/bin/bash
# .bash_profile

export PATH="${HOME}/bin:/usr/local/sbin:$PATH"

# Get the aliases and functions
if [ -f ~/.bashrc ]; then
        source ~/.bashrc
fi

if which rbenv > /dev/null; then eval "$(rbenv init -)"; fi

#export PATH="$PATH:/usr/local/opt/sqlite/bin"
#export LDFLAGS="-L/usr/local/opt/sqlite/lib"
#export CPPFLAGS="-I/usr/local/opt/sqlite/include"
export PKG_CONFIG_PATH="/usr/local/opt/sqlite/lib/pkgconfig"




test -e "${HOME}/.iterm2_shell_integration.bash" && source "${HOME}/.iterm2_shell_integration.bash"


[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*
