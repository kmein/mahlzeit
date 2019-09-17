{ mkDerivation, ansi-terminal, base, directory, doctest, filepath
, megaparsec, optparse-applicative, prettyprinter, process, stdenv
, string-quote, tasty, tasty-hunit, text, yaml
}:
mkDerivation {
  pname = "mahlzeit";
  version = "0.1.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    ansi-terminal base directory filepath megaparsec prettyprinter text
    yaml
  ];
  executableHaskellDepends = [
    ansi-terminal base directory filepath optparse-applicative process
    text yaml
  ];
  testHaskellDepends = [
    base doctest string-quote tasty tasty-hunit
  ];
  homepage = "https://github.com/kmein/mahlzeit";
  description = "Recipe toolkit";
  license = stdenv.lib.licenses.mit;
}
