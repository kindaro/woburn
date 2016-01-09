{ mkDerivation, async, base, containers, diet-set, dlist, free, gtk
, lens, linear, mtl, process, QuickCheck, stdenv, stm, stm-chans
, template-haskell, transformers, wayland-wire
}:
mkDerivation {
  pname = "woburn";
  version = "0.1.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [
    async base containers diet-set dlist free gtk lens linear mtl
    process stm stm-chans template-haskell transformers wayland-wire
  ];
  testDepends = [
    async base containers diet-set dlist free gtk lens linear mtl
    process QuickCheck stm stm-chans template-haskell transformers
    wayland-wire
  ];
  description = "Wayland compositor";
  license = stdenv.lib.licenses.gpl3;
}
