{
  description = "Terminal based speed typing game";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.url = "github:nixos/nixpkgs/22.05";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  };

  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [
          haskellNix.overlay
          (final: prev: {
            web-app = final.haskell-nix.project' {
              src = ./.;
              compiler-nix-name = "ghc98";
              shell.tools = {
                cabal = { };
                hlint = { };
                haskell-language-server = { };
              };
              shell.buildInputs = with pkgs; [ nixpkgs-fmt ghcid ormolu jq ];
            };
          })
        ];
        pkgs = import nixpkgs {
          inherit system overlays;
          inherit (haskellNix) config;
        };
        flake = pkgs.web-app.flake { };
      in flake // {
        packages.default =
          flake.packages."TerminalVelocity:exe:TerminalVelocity";
        nixConfig = {
          extra-substituters = [
            "https://cache.iog.io"
            "https://cache.zw3rk.com"
            "https://cache.nixos.org/"
          ];
          extra-trusted-public-keys = [
            "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
            "loony-tools:pr9m4BkM/5/eSTZlkQyRt57Jz7OMBxNSUiMC4FkcNfk="
            "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
          ];
        };
      });
}
