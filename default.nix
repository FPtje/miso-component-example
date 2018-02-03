{ pkgs ? import <nixpkgs> {} }:
let
  inherit (pkgs.haskell.packages) ghcjsHEAD;

  miso-src = pkgs.fetchFromGitHub {
    rev = "adea51505f30853caae76f38faa5e9f192ae8827";
    sha256 = "0x8dik2cx5j11svb091pzy2ycvhbb88534fng2v9bic5yx1a8c72";
    owner = "haskell-miso";
    repo = "miso";
  };

  miso-ghcjs = ghcjsHEAD.callCabal2nix "miso" miso-src {};

  drv = ghcjsHEAD.callPackage ./pkg.nix { miso = miso-ghcjs; };
in
  if pkgs.lib.inNixShell then drv.env else drv
