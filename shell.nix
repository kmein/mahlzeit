{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  buildInputs = with pkgs; [
    (writers.writeDashBin "render" ''
      ${pandoc}/bin/pandoc \
        --variable=mainfont:Roboto \
        --variable=pagestyle:empty \
        --pdf-engine=xelatex \
        $@
    '')
  ];
  shellHook = ''
    export HISTFILE=${toString ./.history}
    export RECIPE_HOME=${toString ./examples}
  '';
}
