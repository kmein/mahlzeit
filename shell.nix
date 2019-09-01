{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  buildInputs = with pkgs; [ haskellPackages.brittany ];
  shellHook = "export HISTFILE=${toString ./.history}";
}
