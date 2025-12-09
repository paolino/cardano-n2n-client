{ CHaP, indexState, pkgs, mkdocs, asciinema, ... }:

let
  indexTool = { index-state = indexState; };
  fix-libs = { lib, pkgs, ... }: {
    # Use the VRF fork of libsodium
    packages.cardano-crypto-praos.components.library.pkgconfig =
      lib.mkForce [ [ pkgs.libsodium-vrf ] ];
    packages.cardano-crypto-class.components.library.pkgconfig =
      lib.mkForce [[ pkgs.libsodium-vrf pkgs.secp256k1 pkgs.libblst ]];
  };
  shell = { pkgs, ... }: {
    tools = {
      cabal = indexTool;
      cabal-fmt = indexTool;
      haskell-language-server = indexTool;
      hoogle = indexTool;
      fourmolu = indexTool;
      hlint = indexTool;
    };
    withHoogle = true;
    buildInputs = with pkgs; [
      just
      nixfmt-classic
      pkgs.mkdocs
      mkdocs.asciinema-plugin
      mkdocs.markdown-callouts
      asciinema.compress
      asciinema.resize
      pkgs.asciinema

    ];
    shellHook = ''
      echo "Entering shell for cardano-n2n-client development"
    '';
  };
  mkProject = ctx@{ lib, pkgs, ... }: {
    name = "cardano-n2n-client";
    src = ./..;
    compiler-nix-name = "ghc984";

    modules = [ fix-libs ];
    inputMap = { "https://chap.intersectmbo.org/" = CHaP; };
  };

  project = pkgs.haskell-nix.cabalProject' mkProject;

  quality-shell = { pkgs, ... }: {
    tools = {
      cabal-fmt = indexTool;
      fourmolu = indexTool;
      hlint = indexTool;
    };
    withHoogle = false;
    buildInputs = [ pkgs.nixfmt-classic pkgs.just ];
  };

in {
  devShells = {
    default = project.shellFor shell;
    quality = project.shellFor quality-shell;
  };
  packages.cardano-n2n-client =
    project.hsPkgs.cardano-n2n-client.components.exes.cardano-n2n-client;
  packages.cardano-n2n-client-tests =
    project.hsPkgs.cardano-n2n-client.components.tests.cardano-n2n-client-test;
  inherit project;
}
