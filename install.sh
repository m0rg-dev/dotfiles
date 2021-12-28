#!/usr/bin/env bash
cd $(dirname $0)
set -x
for dir in emacs zsh tmux task; do
    cd $dir; ls -A | xargs realpath | xargs -I{} ln -svf {} ~/; cd -
done
test -e ${ZSH_CUSTOM:-$HOME/.oh-my-zsh/custom}/themes/powerlevel10k || git clone --depth=1 https://github.com/romkatv/powerlevel10k.git ${ZSH_CUSTOM:-$HOME/.oh-my-zsh/custom}/themes/powerlevel10k
