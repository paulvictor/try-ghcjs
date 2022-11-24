{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    utils.url = github:numtide/flake-utils;
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    amazonka-repo = {
      url = "github:brendanhay/amazonka/f73a957d05f64863e867cf39d0db260718f0fadd";
      flake = false;
    };
  };
  outputs = inputs@{ self, nixpkgs, utils, amazonka-repo, ... }:
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
            mkDerivation = args: hprev.mkDerivation ({
              doCheck = false;
              doBenchmark = false;
              doHoogle = false;
              doHaddock = false;
              enableLibraryProfiling = false;
              enableExecutableProfiling = false;
            } // args);

            entropy = overrideCabal (hprev.entropy) (drv: {
              libraryHaskellDepends = with hfinal; [ ghcjs-dom jsaddle ];
            }) ;

            streamly = hfinal.streamly_0_8_3; # TODO :Get master

            ghcjs-fetch =
              addBuildDepend
                (doJailbreak (dontCheck hprev.ghcjs-fetch))
                [ hfinal.ghcjs-base ];

            try-ghcjs = hfinal.callCabal2nix "try-ghcjs" ./. {};

            Cabal = hfinal.Cabal_3_4_1_0;

            xml-conduit = dontCheck (hprev.xml-conduit);

            amazonka =  (hfinal.callCabal2nix "amazonka" "${amazonka-repo}/lib/amazonka" {});
            amazonka-core =  (hfinal.callCabal2nix "amazonka-core" "${amazonka-repo}/lib/amazonka-core" {});
            amazonka-s3 =  (hfinal.callCabal2nix "amazonka-s3" "${amazonka-repo}/lib/services/amazonka-s3" {});
            amazonka-sso =  (hfinal.callCabal2nix "amazonka-sso" "${amazonka-repo}/lib/services/amazonka-sso" {});
            amazonka-test =  (hfinal.callCabal2nix "amazonka-test" "${amazonka-repo}/lib/amazonka-test" {});
            amazonka-sts =  (hfinal.callCabal2nix "amazonka-sts" "${amazonka-repo}/lib/services/amazonka-sts" {});
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
