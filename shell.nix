{ pkgs ? import <nixpkgs> {}, emacs ? pkgs.emacs, cask ? pkgs.cask }:
pkgs.mkShell {
  # nix-shell shell.nix -I nixpkgs=channel:nixos-unstable --pure --command "make test"
  buildInputs = [
    pkgs.git
    pkgs.nix
    cask
    pkgs.direnv
    emacs
    pkgs.hyperfine
  ];
}
