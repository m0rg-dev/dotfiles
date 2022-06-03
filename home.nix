{ config, pkgs, ... }:

{
  nixpkgs.config = {
    allowUnfree = true;
  };

  fonts.fontconfig.enable = true;

  home.packages = with pkgs; [
    discord
    iosevka
    neofetch
    rustup
    ncurses
  ];

  programs.emacs = {
    enable = true;
    extraPackages = epkgs: [
      epkgs.use-package
    ];
    package = pkgs.emacs-nox;
  };

  programs.firefox = {
    enable = true;
  };

  programs.git = {
    enable = true;
    userName = "Morgan Wolfe";
    userEmail = "corp@m0rg.dev";
  };

  programs.gh = {
    enable = true;
  };

  programs.starship = {
    enable = true;
  };

  programs.zsh = {
    enable = true;
    enableSyntaxHighlighting = true;
    defaultKeymap = "emacs";
    oh-my-zsh = {
      enable = true;
    };
  };

  home.username = "morgan";
  home.homeDirectory = "/home/morgan";

  home.stateVersion = "21.11";

  programs.home-manager.enable = true;
}
