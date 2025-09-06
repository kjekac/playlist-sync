{
  description = "playlist-dl";

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

          # slskd isn't in nixpkgs on MacOS so we build from source
          slskd-darwin =
            { rev ? "v0.23.2"
            , srcHash ? "sha256-REPLACE_ME_SRC"
            , vendorHash ? "sha256-REPLACE_ME_VENDOR"
            }:
            pkgs.buildDotnetModule rec {
              pname = "slskd";
              version = lib.removePrefix "v" rev;

              src = pkgs.fetchFromGitHub {
                owner = "slskdorg";
                repo  = "slskd";
                rev   = rev;
                hash  = srcHash;
              };

              projectFile        = "src/slskd/slskd.csproj";
              testProjectFile    = null;
              dotnet-sdk         = pkgs.dotnetCorePackages.sdk_8_0;
              dotnet-runtime     = pkgs.dotnetCorePackages.aspnetcore_8_0;
              selfContainedBuild = false;

              vendorHash = vendorHash;

              executables = [ "slskd" ];
            };

          slskdPkg =
            if pkgs.stdenv.isDarwin
            then slskd-darwin { }
            else pkgs.slskd;

          # playlist-dl
          hpkgs = pkgs.haskellPackages;

          playlist-dl = pkgs.haskell.lib.overrideCabal
            (hpkgs.callCabal2nix "playlist-dl" ./. { })
            (old: {
              librarySystemDepends    = (old.librarySystemDepends or [])    ++ [ pkgs.taglib pkgs.zlib ];
              executableSystemDepends = (old.executableSystemDepends or []) ++ [ pkgs.taglib pkgs.zlib ];
              buildTools              = (old.buildTools or [])              ++ [ pkgs.pkg-config ];
            });

          # playlist-dl with slskd on the path
          playlist-dl-with-slskd = pkgs.stdenvNoCC.mkDerivation {
            pname = "playlist-dl-with-slskd";
            version = "wrapped";
            dontUnpack = true;
            nativeBuildInputs = [ pkgs.makeWrapper ];
            installPhase = ''
              mkdir -p $out/bin
              makeWrapper ${playlist-dl}/bin/playlist-dl \
                $out/bin/playlist-dl \
                --prefix PATH : "${pkgs.lib.makeBinPath [ slskdPkg ]}"
            '';
          };
        in
        {
          packages = {
            default               = playlist-dl-with-slskd;
            playlist-dl           = playlist-dl;
            slskd                 = slskdPkg;
          };

          apps = {
            default = { type = "app"; program = "${playlist-dl-with-slskd}/bin/playlist-dl"; };
            playlist-dl = { type = "app"; program = "${playlist-dl}/bin/playlist-dl"; };
            slskd = { type = "app"; program = "${slskdPkg}/bin/slskd"; };
          };

          devShells.default = pkgs.mkShell {
            buildInputs =
              [ pkgs.cabal-install pkgs.pkg-config pkgs.taglib pkgs.zlib ]
              ++ lib.optionals pkgs.stdenv.isDarwin [
                pkgs.dotnetCorePackages.sdk_8_0
                pkgs.dotnetCorePackages.aspnetcore_8_0
              ];
          };
        });
}

