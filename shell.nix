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
    # (haskellPackages.callPackage (import ./default.nix) {})
  ];
  shellHook = ''
    export HISTFILE=${toString ./.history}
    export RECIPE_HOME=${toString ./examples}
  '';
}
