{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  # nix-shell shell.nix -I nixpkgs=channel:nixos-unstable --pure --command "make test"
  buildInputs = [
    pkgs.git
    pkgs.nix
    pkgs.cask
    pkgs.coreutils
    pkgs.which
    pkgs.python3
    pkgs.direnv
    pkgs.emacs
    pkgs.emacsPackages.org
  ];
}
