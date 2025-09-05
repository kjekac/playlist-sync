{ pkgs ? import <nixpkgs> {} }:
let
  hpkgs = pkgs.haskellPackages;
  drv0 = hpkgs.callCabal2nix "playlist-dl" ./. {};
  drv  = pkgs.haskell.lib.overrideCabal drv0 (old: {
    librarySystemDepends    = (old.librarySystemDepends or [])    ++ [ pkgs.taglib pkgs.zlib ];
    executableSystemDepends = (old.executableSystemDepends or []) ++ [ pkgs.taglib pkgs.zlib ];
    buildTools              = (old.buildTools or [])              ++ [ pkgs.pkg-config ];
  });
in {
  playlist-dl = drv;                # nix-build -A playlist-dl
  shell = hpkgs.shellFor {          # nix-shell  -A shell
    packages   = p: [ drv ];
    buildInputs = [ pkgs.cabal-install pkgs.pkg-config pkgs.taglib pkgs.zlib ];
  };
}

