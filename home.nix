{ config, pkgs, ... }:

{
  nixpkgs.config = {
    allowUnfree = true;
  };

  fonts.fontconfig.enable = true;

  home.packages = with pkgs; [
    discord
    emacs
    iosevka
    neofetch
  ];

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

  home.stateVersion = "21.05";

  programs.home-manager.enable = true;
}
