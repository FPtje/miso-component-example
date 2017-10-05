{ pkgs ? import <nixpkgs> {} }:
let
  inherit (pkgs.haskell.packages) ghcjsHEAD;

  miso-src = pkgs.fetchFromGitHub {
    sha256 = "1ha0nh9jmy6czlikdb0fay03mir2gyx2dj07lzwfvap7hx7gjb6n";
    rev = "1593151dd10f26c14dd2cf7cea3489f27a6d8f58";
    owner = "haskell-miso";
    repo = "miso";
  };

  miso-ghcjs = ghcjsHEAD.callCabal2nix "miso" miso-src {};

  drv = ghcjsHEAD.callPackage ./pkg.nix { miso = miso-ghcjs; };
in
  if pkgs.lib.inNixShell then drv.env else drv
