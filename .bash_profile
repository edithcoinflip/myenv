#!/bin/bash
# .bash_profile

# Get the aliases and functions
if [ -f ~/.bashrc ]; then
        source ~/.bashrc
fi

export PATH="${HOME}/bin:/usr/local/sbin:$PATH"

if which rbenv > /dev/null; then eval "$(rbenv init -)"; fi

#export PATH="$PATH:/usr/local/opt/sqlite/bin"
#export LDFLAGS="-L/usr/local/opt/sqlite/lib"
#export CPPFLAGS="-I/usr/local/opt/sqlite/include"
export PKG_CONFIG_PATH="/usr/local/opt/sqlite/lib/pkgconfig"



