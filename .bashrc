# Colorful fortunes upon login if the shell is interactive
# If you don't check interactive it breaks scp
case "$-" in
    *)	fortune | cowsay -f $(ls /usr/share/cowsay/cows/ | shuf -n1) | lolcat;;
esac

alias emacs='emacs -nw'
alias python=python3
alias pip=pip3
alias tunnel=./.scripts/QuickTunnel.sh

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
