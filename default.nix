{ mkDerivation, base, containers, diet-set, linear, mtl, QuickCheck, async
, stdenv, wayland-wire, vimEnv, cabal-install, wayland, pkgconfig, hlint
, git, less, ghc-mod, stm, stm-chans
}:
mkDerivation {
  pname = "woburn";
  version = "0.1.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildTools = [ vimEnv cabal-install pkgconfig hlint git less ghc-mod ];
  buildDepends = [
    base containers diet-set linear mtl wayland-wire wayland async stm stm-chans
  ];
  testDepends = [
    base containers diet-set linear mtl QuickCheck wayland-wire async stm stm-chans
  ];
  description = "Wayland compositor";
  license = stdenv.lib.licenses.gpl3;
}
