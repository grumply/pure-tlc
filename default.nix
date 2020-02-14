{ mkDerivation, base, stdenv }:
mkDerivation {
  pname = "pure-tlc";
  version = "0.8.0.0";
  src = ./.;
  libraryHaskellDepends = [ base ];
  homepage = "github.com/grumply/pure-tlc";
  license = stdenv.lib.licenses.bsd3;
}
