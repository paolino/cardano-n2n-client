{ pkgs, project, version, ... }:

pkgs.dockerTools.buildImage {
  name =
    "ghcr.io/cardano-foundation/cardano-node-cardano-n2n-client/cardano-n2n-client";
  tag = version;
  config = { EntryPoint = [ "cardano-n2n-client" ]; };
  copyToRoot = pkgs.buildEnv {
    name = "image-root";
    paths = [
      project.packages.cardano-n2n-client.package.components.exes.cardano-n2n-client
    ];
  };
}
