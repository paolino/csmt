{ pkgs, project, version, ... }:

pkgs.dockerTools.buildImage {
  name = "ghcr.io/paolino/csmt/csmt";
  tag = version;
  config = { EntryPoint = [ "csmt" ]; };
  copyToRoot = pkgs.buildEnv {
    name = "image-root";
    paths = [ project.packages.csmt.package.components.exes.csmt ];
  };
}
