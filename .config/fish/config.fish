set PATH $HOME/bin /usr/local/sbin $PATH

if test (which rbenv > /dev/null)
   eval (rbenv init -)
end
   	

#export PATH="$PATH:/usr/local/opt/sqlite/bin"
#export LDFLAGS="-L/usr/local/opt/sqlite/lib"
#export CPPFLAGS="-I/usr/local/opt/sqlite/include"
export PKG_CONFIG_PATH="/usr/local/opt/sqlite/lib/pkgconfig"

#export EDITOR=emacs

#test -x /usr/bin/lesspipe; and eval "$(SHELL=/bin/sh lesspipe)"

source ~/.iterm2_shell_integration.fish
function iterm2_print_user_vars
    set -l git_branch (git branch 2> /dev/null | sed -n '/\* /s///p')
    iterm2_set_user_var gitBranch "$git_branch"
end
