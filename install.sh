ls -A emacs | xargs realpath | xargs -I{} ln -svf {} ~/
ls -A zsh | xargs realpath | xargs -I{} ln -svf {} ~/
