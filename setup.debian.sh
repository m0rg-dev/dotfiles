#!/usr/bin/env bash

sudo apt-get update
sudo apt-get install git stow zsh

git clone https://github.com/digitalagedragon/dotfiles
cd dotfiles
stow zsh
