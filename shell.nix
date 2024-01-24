{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  buildInputs = [
    pkgs.git
    pkgs.cask
    pkgs.coreutils
    pkgs.which
    pkgs.python3
    pkgs.direnv
    pkgs.emacs
    pkgs.emacsPackages.org
  ];
}
