{ pkgs ? import <nixpkgs> {} }:
with pkgs.haskellPackages;
developPackage {
  root = ./.;
  modifier = drv:
  pkgs.haskell.lib.addBuildTools drv
    [ cabal-install
      ghcid
      (pkgs.writers.writeDashBin "render" ''
        ${pandoc}/bin/pandoc \
          --variable=fontfamily:libertine \
          --variable=documentclass:scrartcl \
          --variable=pagestyle:empty \
          $@
      '')
  ];
}
