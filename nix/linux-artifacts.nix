{ pkgs, version, project, ... }:
let
  csmt = project.musl64.csmt.components.exes.csmt;
  tarball-derivation = pkgs.stdenv.mkDerivation rec {
    pname = "csmt";
    inherit version;
    unpackPhase = ''
      mkdir -p $out/unpacked
      cp ${csmt}/bin/csmt $out/unpacked
      chmod -R +w $out/unpacked/*
    '';
    installPhase = ''
      tar -C $out/unpacked -czvf $out/$pname-$version-linux64.tar.gz .
      rm -rf $out/unpacked
    '';
  };
in { packages.linux64.tarball = tarball-derivation; }
