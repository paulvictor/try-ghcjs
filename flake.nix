{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    utils.url = github:numtide/flake-utils;
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };
  outputs = inputs@{ self, nixpkgs, utils, ... }:
    utils.lib.eachDefaultSystem (system:
      let
        ghcFor = pkgs: pkgs.haskell.packages.ghcjs;
        emscripten-overlay = final: prev: {
          emscripten = prev.emscripten.overrideAttrs(_: {
            version = "3.1.14";
            src = final.fetchFromGitHub {
              owner = "emscripten-core";
              repo = "emscripten";
              sha256 = "sha256-CVFC278ibwUMib2F64Uc7FP+D1JPUJ/9/3w0wz1PWqg=";
              rev = "3.1.14";
            };
          });
        };
        pkgs =
          import nixpkgs {
            inherit system overlays;
            config = { allowBroken = true; };
          };
        haskellOverrides = hfinal: hprev:
          with pkgs.haskell.lib;
          {
            ghcjs-fetch =
              addBuildDepend
                (doJailbreak (dontCheck hprev.ghcjs-fetch))
                [ hfinal.ghcjs-base ];
            try-ghcjs = hfinal.callCabal2nix "try-ghcjs" ./. {};
          };
        overlays = [ emscripten-overlay ];
        hp = ghcFor pkgs;
        shellDeps = hp:
          with hp;
          [ cabal-install
          ];
        try-ghcjs = (hp.extend haskellOverrides).try-ghcjs;

      in
        {
          inherit try-ghcjs ghcFor hp overlays haskellOverrides;
          devShell =
            (hp.extend haskellOverrides).shellFor {
              packages = p: [ p.try-ghcjs ];
              nativeBuildInputs = with pkgs; [ pkgs.cabal-install ];
          };
          legacyPackages = pkgs;
        }
    );
}
