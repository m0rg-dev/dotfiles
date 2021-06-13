#!/bin/sh

cd $(dirname $0)

git submodule init
git submodule update

for program in emacs zsh code-oss void-specific; do
    echo "[Installing: $program]"
    if [ -x $program/install ]; then
        $program/install
    else
        for f in $(ls -A $program); do
            echo ln -sfv $(realpath $program/$(basename $f)) ~/$(basename $f)
        done
    fi
done
