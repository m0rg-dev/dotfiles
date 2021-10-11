cd emacs; ls -A | xargs realpath | xargs -I{} ln -svf {} ~/; cd -
cd zsh; ls -A | xargs realpath | xargs -I{} ln -svf {} ~/; cd -
