{ pkgs ? import <nixpkgs> {} }:
let
  inherit (pkgs.haskell.packages) ghcjsHEAD;

  miso-src = pkgs.fetchFromGitHub {
    rev = "9b6d7484d7e85d3052eac2e2cc4bc482f593bd57";
    sha256 = "15dkphq878rip1dyi4nbgbzp97jn8v7d2b0iy0ckdxar35v51ywv";
    owner = "haskell-miso";
    repo = "miso";
  };

  miso-ghcjs = ghcjsHEAD.callCabal2nix "miso" miso-src {};

  drv = ghcjsHEAD.callPackage ./pkg.nix { miso = miso-ghcjs; };
in
  if pkgs.lib.inNixShell then drv.env else drv
