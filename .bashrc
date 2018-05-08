# If not running interactively, don't do anything
# prevents RC from breaking non-interactive commands
# like SCP
case $- in
    *i*) ;;
    *) return;;
esac 

# Colorful fortunes upon login if the shell is interactive
fortune | cowsay -f $(ls /usr/share/cowsay/cows/ | shuf -n1) | lolcat;;

alias emacs='emacs -nw'
alias python=python3
alias pip=pip3
alias tunnel=./.scripts/QuickTunnel.sh
alias session="tmux new -s \$1"
alias workon="tmux attach -t \$1"

# Defer initialization of nvm until nvm, node or a node-dependent command is
if [ -s "$HOME/.nvm/nvm.sh" ]; then
    export NVM_DIR="$HOME/.nvm"
    alias load_nvm='[ -s "$NVM_DIR/bash_completion" ] && . "$NVM_DIR/bash_completion" && unalias nvm node npm && . "$NVM_DIR"/nvm.sh'
    alias nvm='load_nvm && nvm'
    alias node='load_nvm && node'
    alias npm='load_nvm && npm'
fi

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

git config --global user.name "Helw150"
git config --global user.email wbh230@nyu.edu
export TERM=xterm-256color
