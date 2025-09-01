{ pkgs ? import <nixpkgs> {} }:
pkgs.haskellPackages.callCabal2nix "playlist-dl" ./. {
  buildInputs = [ pkgs.curl pkgs.jq ];
  configureFlags = [ "--ghc-options=-threaded" ];
}

