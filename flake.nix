{
  description = "playlist-sync";

  inputs = {
    nixpkgs.url     = "github:NixOS/nixpkgs/nixos-25.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachSystem
      [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ]
      (system:
        let
          pkgs = import nixpkgs { inherit system; };
          lib  = pkgs.lib;

          # we pin an old version to guarantee that the Haskell library works everywhere.
          taglib_1_13 = pkgs.taglib.overrideAttrs (old: {
            version = "1.13";
            src = pkgs.fetchurl {
              url = "https://taglib.github.io/releases/taglib-1.13.tar.gz";
              sha256 = "sha256-WPCLTbPcMe0VLASJbukXLSIFK8fvEoiAKMAdix1greA=";
            };
          });

          # slskd isn't in nixpkgs on MacOS so we build from source
          slskd-darwin =
            { rev ? "0.23.2"
            , srcHash ? "sha256-y/qgx4tC7QGQTbBqhvp/TUalW1MfKYzvvSVgyNvKMME="
            }:
            pkgs.buildDotnetModule {
              pname = "slskd";
              version = rev;

              src = pkgs.fetchFromGitHub {
                owner = "slskd";
                repo  = "slskd";
                rev   = rev;
                hash  = srcHash;
              };

              projectFile        = "src/slskd/slskd.csproj";
              testProjectFile    = null;
              dotnet-sdk         = pkgs.dotnetCorePackages.sdk_8_0;
              dotnet-runtime     = pkgs.dotnetCorePackages.aspnetcore_8_0;
              selfContainedBuild = false;

              nugetDeps = ./slskd-deps.json;

              executables = [ "slskd" ];
          };

          slskdPkg =
            if pkgs.stdenv.isDarwin
            then slskd-darwin { }
            else pkgs.slskd;

          # playlist-sync
          hpkgs = pkgs.haskellPackages;

          playlist-sync = pkgs.haskell.lib.overrideCabal
            (hpkgs.callCabal2nix "playlist-sync" ./. { })
            (old: {
              librarySystemDepends    = (old.librarySystemDepends or [])    ++ [ taglib_1_13 pkgs.zlib ];
              executableSystemDepends = (old.executableSystemDepends or []) ++ [ taglib_1_13 pkgs.zlib ];
              buildTools              = (old.buildTools or [])              ++ [ pkgs.pkg-config ];
            });

          # playlist-sync with slskd on the path
          playlist-sync-with-slskd = pkgs.stdenvNoCC.mkDerivation {
            pname = "playlist-sync-with-slskd";
            version = "wrapped";
            dontUnpack = true;
            nativeBuildInputs = [ pkgs.makeWrapper ];
            installPhase = ''
              mkdir -p $out/bin
              makeWrapper ${playlist-sync}/bin/playlist-sync \
                $out/bin/playlist-sync \
                --prefix PATH : "${pkgs.lib.makeBinPath [ slskdPkg ]}"
            '';
          };
        in
        {
          packages = {
            default               = playlist-sync-with-slskd;
            playlist-sync         = playlist-sync;
            slskd                 = slskdPkg;
          };

          apps = {
            default       = { type = "app"; program = "${playlist-sync-with-slskd}/bin/playlist-sync"; };
            playlist-sync = { type = "app"; program = "${playlist-sync}/bin/playlist-sync"; };
            slskd         = { type = "app"; program = "${slskdPkg}/bin/slskd"; };
          };

          devShells.default = pkgs.mkShell {
            inputsFrom = [ playlist-sync.env ];
            buildInputs = [
              pkgs.cabal-install
              pkgs.pkg-config
              taglib_1_13
              pkgs.zlib
            ] ++ lib.optionals pkgs.stdenv.isDarwin [
              pkgs.dotnetCorePackages.sdk_8_0
              pkgs.dotnetCorePackages.aspnetcore_8_0
            ];
          };
        });
}

