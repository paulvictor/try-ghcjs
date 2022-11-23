let
  emscripten-overlay = final: prev: {
    emscripten = prev.emscripten.overrideAttrs(_: {
      version = "3.1.14";
      src = final.fetchFromGitHub {
        owner = "emscripten-core";
        repo = "emscripten";
        sha256 = "sha256-CVFC278ibwUMib2F64Uc7FP+D1JPUJ/9/3w0wz1PWqg="; rev = "3.1.14";
      };
    });
  };
  pkgs = import <nixpkgs> { overlays = [ emscripten-overlay ]; };

in
with pkgs;
mkShell {
  name = "init-sh";
  buildInputs = [ cabal-install haskell.compiler.ghcjs haskell.compiler.ghc8107 ];
}
