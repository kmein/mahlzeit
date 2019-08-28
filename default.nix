{ mkDerivation, base, megaparsec, stdenv, text }:
mkDerivation {
  pname = "mahlzeit";
  version = "0.1.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base megaparsec text ];
  homepage = "https://github.com/kmein/mahlzeit";
  description = "Recipe parser";
  license = stdenv.lib.licenses.mit;
}
