{ mkDerivation, base, lens, miso, stdenv }:
mkDerivation {
  pname = "miso-components-example";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base lens miso ];
  description = "Components example for Miso";
  license = stdenv.lib.licenses.unfree;
}
