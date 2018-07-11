# If not running interactively, don't do anything
# prevents RC from breaking non-interactive commands
# like SCP
case $- in
    *i*) ;;
    *) return;;
esac 

if ! ssh-add -l >/dev/null; then
    eval `ssh-agent`
    ssh-add ~/.ssh/id_rsa
    ssh-add ~/.ssh/will_held
fi

# Fancy Bash Prompt
if [ -f "$(brew --prefix)/opt/bash-git-prompt/share/gitprompt.sh" ]; then
  __GIT_PROMPT_DIR=$(brew --prefix)/opt/bash-git-prompt/share
  GIT_PROMPT_ONLY_IN_REPO=1
  source "$(brew --prefix)/opt/bash-git-prompt/share/gitprompt.sh"
fi

# Colorful fortunes upon login if the shell is interactive
fortune | cowsay -f $(ls /usr/share/cowsay/cows/ | shuf -n1) | lolcat

# Emacs without GUI
alias emacs='emacs -nw'

# Hack to make python 3
alias python=python3
alias pip=pip3

# Quickly make a reverse tunnel
alias tunnel=./.scripts/QuickTunnel.sh

# Alias to wrap connecting and creating tmux workspaces
workon() {
    tmux new -s $@ || tmux attach -t $@
}

# LS Aliases
alias ls='ls --color=auto'
alias tls="tmux list-sessions"
alias hls="hadoop fs -ls"

# Totally insecure and convenient notebook
alias jn='scripts/jupyter/run.sh notebook --NotebookApp.ip='*' --NotebookApp.password='' --NotebookApp.token='' --notebook-dir=~/notebooks/'


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
