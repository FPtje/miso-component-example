{ pkgs ? import ./nixpkgs.nix }:
let
  inherit (pkgs.haskell.packages) ghcjs;

  drv = ghcjs.callCabal2nix "miso-components-example" ./. {};
in
  if pkgs.lib.inNixShell then drv.env else drv
