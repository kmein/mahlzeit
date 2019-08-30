{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, ansi-terminal, base, directory, filepath
      , megaparsec, optparse-applicative, pretty, prettyprinter, stdenv
      , text, yaml
      }:
      mkDerivation {
        pname = "mahlzeit";
        version = "0.1.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          ansi-terminal base directory filepath megaparsec
          optparse-applicative pretty prettyprinter text yaml
        ];
        homepage = "https://github.com/kmein/mahlzeit";
        description = "Recipe toolkit";
        license = stdenv.lib.licenses.mit;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
