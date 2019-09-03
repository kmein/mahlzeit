{ mkDerivation, ansi-terminal, base, directory, filepath
, megaparsec, optparse-applicative, prettyprinter, process, stdenv
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
    optparse-applicative prettyprinter process text yaml
  ];
  homepage = "https://github.com/kmein/mahlzeit";
  description = "Recipe toolkit";
  license = stdenv.lib.licenses.mit;
}
