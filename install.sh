#!/usr/bin/env bash
cd $(dirname $0)
set -x
for dir in emacs zsh tmux task; do
    cd $dir; ls -A | xargs realpath | xargs -I{} ln -svf {} ~/; cd -
done

mkdir -pv ~/.config && ln -s $(realpath starship.toml) ~/.config/starship.toml

rm -rf $HOME/.oh-my-zsh/custom/themes/powerlevel10k
cp .gitignore .stignore
