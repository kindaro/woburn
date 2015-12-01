{ mkDerivation, base, containers, diet-set, linear, mtl, QuickCheck, async
, stdenv, wayland-wire, vimEnv, cabal-install, wayland, pkgconfig, hlint
, git, less, ghc-mod, stm, stm-chans, dlist, gtk, ghc-events, threadscope
}:
mkDerivation {
  pname = "woburn";
  version = "0.1.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildTools = [ vimEnv cabal-install pkgconfig hlint git less ghc-mod ghc-events threadscope ];
  buildDepends = [
    base containers diet-set linear mtl wayland-wire wayland async stm stm-chans dlist gtk
  ];
  testDepends = [
    base containers diet-set linear mtl QuickCheck wayland-wire async stm stm-chans dlist gtk
  ];
  description = "Wayland compositor";
  license = stdenv.lib.licenses.gpl3;
}
