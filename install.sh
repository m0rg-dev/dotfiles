cd $(dirname $0)
cd emacs; ls -A | xargs realpath | xargs -I{} ln -svf {} ~/; cd -
cd zsh; ls -A | xargs realpath | xargs -I{} ln -svf {} ~/; cd -
test -n ${ZSH_CUSTOM:-$HOME/.oh-my-zsh/custom}/themes/powerlevel10k && git clone --depth=1 https://github.com/romkatv/powerlevel10k.git ${ZSH_CUSTOM:-$HOME/.oh-my-zsh/custom}/themes/powerlevel10k
