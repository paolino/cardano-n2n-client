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
    cardano-node.url = "github:IntersectMBO/cardano-node";
    mkdocs.url = "github:paolino/dev-assets?dir=mkdocs";
    asciinema.url = "github:paolino/dev-assets?dir=asciinema";
  };

  outputs = inputs@{ self, nixpkgs, flake-parts, haskellNix, CHaP, iohkNix
    , mkdocs, asciinema, cardano-node, ... }:
    let
      version = self.dirtyShortRev or self.shortRev;
      parts = flake-parts.lib.mkFlake { inherit inputs; } {
        systems = [ "x86_64-linux" "aarch64-darwin" ];
        perSystem = { system, ... }:
          let
            pkgs = import nixpkgs {
              overlays = [
                iohkNix.overlays.crypto # modified crypto libs
                haskellNix.overlay # some functions
                iohkNix.overlays.haskell-nix-crypto
                iohkNix.overlays.cardano-lib
              ];
              inherit system;
            };
            project = pkgs.callPackage ./nix/project.nix {
              inherit CHaP;
              cardano-node = cardano-node.packages.${system};
              indexState = "2025-08-07T00:00:00Z";
              mkdocs = mkdocs.packages.${system};
              asciinema = asciinema.packages.${system};
            };
            docker-image = pkgs.callPackage ./nix/docker-image.nix {
              inherit project version;
            };

          in rec {
            packages = {
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
