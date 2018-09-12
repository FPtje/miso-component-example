{ pkgs ? import <nixpkgs> {} }:
let
  inherit (pkgs.haskell.packages) ghcjsHEAD;

  miso-src = pkgs.fetchFromGitHub {
    rev = "bb2be3264ff3c6aa3b18e471d7cf04296024059b";
    sha256 = "07k1rlvl9g027fp2khl9kiwla4rcn9sv8v2dzm0rzf149aal93vn";
    owner = "haskell-miso";
    repo = "miso";
  };

  miso-ghcjs = ghcjsHEAD.callCabal2nix "miso" miso-src {};

  drv = ghcjsHEAD.callPackage ./pkg.nix { miso = miso-ghcjs; };
in
  if pkgs.lib.inNixShell then drv.env else drv
