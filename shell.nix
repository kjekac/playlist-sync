{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  inputsFrom = [ (import ./default.nix { inherit pkgs; }) ];
  buildInputs = [
    pkgs.haskellPackages.ghc
  ];
}

