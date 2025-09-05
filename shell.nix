{ pkgs ? import <nixpkgs> {} }:
let d = import ./default.nix { inherit pkgs; };
in d.shell

