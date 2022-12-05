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
    xml-hs-repo = {
      url = "github:snoyberg/xml/master";
      flake = false;
    };
    streamly-repo = {
      url = "github:composewell/streamly/master";
      flake = false;
    };
    network-hs-repo = {
      url = "github:haskell/network/master";
      flake = false;
    };
  };
  outputs = inputs@{ self, amazonka-repo, ... }:
    inputs.utils.lib.eachDefaultSystem (system:
      let
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
        overlays = [ emscripten-overlay ];
        legacyPackages =
          import inputs.nixpkgs {
            inherit system overlays;
            config = { allowBroken = true; };
          };
        xml-conduit-patch = legacyPackages.fetchurl {
          url = "https://patch-diff.githubusercontent.com/raw/snoyberg/xml/pull/171.patch";
          sha256 = "1kb38dys0q2qmrwx1scajbfk01v07cj90k5fbmkg6fgl9j278jwm";
        };
        # Check https://github.com/NixOS/nixpkgs/pull/151689 for ghcjs
        forGHC = { pkgs ? legacyPackages, ghc }:
          let
            isGhcjs = pkgs.haskell.compiler.${ghc}.isGhcjs or false;
            nixlib = pkgs.lib;
            haskellOverrides = hfinal: hprev:
              with pkgs.haskell.lib;
              {
                mkDerivation = args: hprev.mkDerivation (
                  (nixlib.optionalAttrs
                    isGhcjs
                    {
                      doCheck = false;
                      doBenchmark = false;
                      doHoogle = false;
                      doHaddock = false;
                      enableLibraryProfiling = false;
                      enableExecutableProfiling = false;
                    }) // args);

                entropy = overrideCabal (hprev.entropy) (drv:
                  nixlib.optionalAttrs isGhcjs {
                    libraryHaskellDepends =
                      with hfinal; [ ghcjs-dom jsaddle ];
                  });

                streamly-core = doJailbreak (hfinal.callCabal2nix "streamly-core" "${inputs.streamly-repo}/core" {});

                streamly = hfinal.callCabal2nix "streamly" inputs.streamly-repo {};

                ghcjs-base =
                  let
                    removeOldAeson = x:
                      with nixlib;
                      overrideCabal x (old: {
                        libraryHaskellDepends = filter(drv: drv == null ||  !(hasPrefix "aeson" drv.name)) x.getCabalDeps.libraryHaskellDepends;
                      });
                  in
                    assert nixlib.assertMsg isGhcjs "ghcjs-base only for ghcjs, not for ${toString ghc}";
                      addBuildDepend
                        (removeOldAeson hprev.ghcjs-base)
                        hfinal.aeson;

                ghcjs-fetch =
                  assert nixlib.assertMsg isGhcjs "ghcjs-fetch only for ghcjs, not for ${toString ghc}";
                    addBuildDepend
                      (doJailbreak (hprev.ghcjs-fetch))
                      [ hfinal.ghcjs-base ];

                network = overrideCabal (hprev.network) (drv: {
                  src = inputs.network-hs-repo;
                  preCompileBuildDriver = ''
                    ${pkgs.autoconf}/bin/autoreconf -i
                  '';
                });

                try-ghcjs =
                  let
                    drv = hfinal.callCabal2nix "try-ghcjs" ./. {};
                  in
                    if isGhcjs
                    then
                      overrideCabal drv (old: {
                        postInstall =
                          (old.postInstall or "") +
                          ''${pkgs.closurecompiler}/bin/closure-compiler $out/bin/try-ghcjs.jsexe/all.js -O ADVANCED  --jscomp_off=checkVars --externs=$out/bin/try-ghcjs.jsexe/all.js.externs > $out/bin/try-ghcjs.jsexe/all.min.js'';
                      })
                    else drv;

                Cabal = hfinal.Cabal_3_4_1_0;

                xml-conduit =
                  if isGhcjs
                  then
                    appendPatch (overrideSrc hprev.xml-conduit { src = "${inputs.xml-hs-repo}/xml-conduit"; }) [ ./nix/171.patch ]
                  else
                    hprev.xml-conduit;

                amazonka =  (hfinal.callCabal2nix "amazonka" "${amazonka-repo}/lib/amazonka" {});
                amazonka-core =  (hfinal.callCabal2nix "amazonka-core" "${amazonka-repo}/lib/amazonka-core" {});
                amazonka-s3 =  (hfinal.callCabal2nix "amazonka-s3" "${amazonka-repo}/lib/services/amazonka-s3" {});
                amazonka-sso =  (hfinal.callCabal2nix "amazonka-sso" "${amazonka-repo}/lib/services/amazonka-sso" {});
                amazonka-test =  (hfinal.callCabal2nix "amazonka-test" "${amazonka-repo}/lib/amazonka-test" {});
                amazonka-sts =  (hfinal.callCabal2nix "amazonka-sts" "${amazonka-repo}/lib/services/amazonka-sts" {});
              };
            shellDeps =
              let
                haskellPkgs = legacyPackages.haskell.packages.${ghc};
              in
                [ legacyPackages.cabal-install ] ++
                pkgs.lib.optional
                  (!isGhcjs)
                  (with haskellPkgs;
                    [ (legacyPackages.haskell.lib.overrideCabal ghcid (_: { enableSeparateBinOutput = false; }))
                      hlint
                      legacyPackages.nixpkgs-fmt ]);
          in { inherit haskellOverrides shellDeps; };
        haskellOverridesFor = ghc: (forGHC { inherit ghc; }).haskellOverrides;
        haskellPkgs = ghc: legacyPackages.haskell.packages.${ghc}.extend (haskellOverridesFor ghc);
        shellFor = ghc:
          (haskellPkgs ghc).shellFor {
            packages = p: [ p.try-ghcjs ];
            withHoogle = (ghc != "ghcjs");
            nativeBuildInputs = (forGHC { inherit ghc; }).shellDeps;
          };
        compilers =
          [ "ghc8107" "ghcjs" ];
      in
        {
          packages.tryghc-js =
            builtins.listToAttrs
              (map (c: legacyPackages.lib.nameValuePair c (haskellPkgs c).try-ghcjs) compilers);
          devShells =
            let
              forCompilers =
                builtins.listToAttrs
                  (map (c: legacyPackages.lib.nameValuePair c (shellFor c)) compilers);
            in (forCompilers // { default = forCompilers.ghc8107; });
#           devShells.default = self.devShells.ghc8107;
          inherit legacyPackages;
        }
    );
}
