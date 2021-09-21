{
  description = "An advent of code project from 2017";
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
    let
      overlays = [ haskellNix.overlay
        (final: prev: {
          # This overlay adds our project to pkgs
          advent-of-code =
            final.haskell-nix.project' {
              src = ./.;
              compiler-nix-name = "ghc8105";
              shell.tools = {
                cabal = {};
                ghcid = {};
                haskell-language-server = {};
                hlint = {};
              };
              shell.withHoogle = true;
              shell.exactDeps = true;
              modules = [{
                # ShellCheck Haddock broken on 0.7.2: https://github.com/koalaman/shellcheck/issues/2216
                packages.ShellCheck.doHaddock = false;
              }];
            };
        })
      ];
      pkgs = import nixpkgs {
        inherit system overlays;
      };
      flake = pkgs.advent-of-code.flake {
        crossPlatforms = p: [];
      };
    in flake // {
      defaultPackage = flake.packages."advent-of-code:lib:advent-of-code";
    });
}
