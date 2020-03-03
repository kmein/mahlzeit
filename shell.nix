{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  buildInputs = with pkgs; [
    (writers.writeDashBin "render" ''
      ${pandoc}/bin/pandoc \
        --variable=fontfamily:libertine \
        --variable=documentclass:scrartcl \
        --variable=pagestyle:empty \
        $@
    '')
  ];

  RECIPE_HOME = ./examples;
}
