{
  description = "cardano-n2n-client";
  nixConfig = {
    extra-substituters = [ "https://cache.iog.io" ];
    extra-trusted-public-keys =
      [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
  };
  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    flake-utils.url = "github:hamishmack/flake-utils/hkm/nested-hydraJobs";
    iohkNix = {
      url = "github:input-output-hk/iohk-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    CHaP = {
      url = "github:intersectmbo/cardano-haskell-packages?ref=repo";
      flake = false;
    };
    cardano-node-runtime = {
      url = "github:IntersectMBO/cardano-node?ref=10.1.4";
    };
    mkdocs.url = "github:paolino/dev-assets?dir=mkdocs";
  };

  outputs = inputs@{ self, nixpkgs, flake-parts, haskellNix, CHaP, iohkNix
    , mkdocs, cardano-node-runtime, ... }:
    let
      version = self.dirtyShortRev or self.shortRev;
      parts = flake-parts.lib.mkFlake { inherit inputs; } {
        systems = [ "x86_64-linux" "aarch64-darwin" ];
        perSystem = { system, ... }:
          let
            node = cardano-node-runtime.project.${system};
            pkgs = import nixpkgs {
              overlays = [
                iohkNix.overlays.crypto # modified crypto libs
                haskellNix.overlay # some functions
                iohkNix.overlays.haskell-nix-crypto
                iohkNix.overlays.cardano-lib
              ];
              inherit system;
            };
            project = import ./nix/project.nix {
              indexState = "2025-08-07T00:00:00Z";
              inherit CHaP;
              inherit pkgs;
              mkdocs = mkdocs.packages.${system};
            };
            docker-image =
              import ./nix/docker-image.nix { inherit pkgs project version; };

          in rec {
            packages = {
              inherit (node.pkgs) cardano-node cardano-cli;
              inherit (project.packages) cardano-n2n-client;
              inherit docker-image;
              default = packages.cardano-n2n-client;
            };
            inherit (project) devShells;
          };
      };
    in {
      inherit (parts) packages devShells;
      inherit version;
    };
}
