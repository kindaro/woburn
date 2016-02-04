{ mkDerivation, async, base, bindings-posix, containers, diet-set
, dlist, filepath, free, gtk, lens, linear, mtl, process
, QuickCheck, stdenv, stm, stm-chans, template-haskell, time
, transformers, wayland-wire
}:
mkDerivation {
  pname = "woburn";
  version = "0.1.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [
    async base bindings-posix containers diet-set dlist filepath free
    gtk lens linear mtl process stm stm-chans template-haskell time
    transformers wayland-wire
  ];
  testDepends = [
    async base bindings-posix containers diet-set dlist filepath free
    gtk lens linear mtl process QuickCheck stm stm-chans
    template-haskell time transformers wayland-wire
  ];
  description = "Wayland compositor";
  license = stdenv.lib.licenses.gpl3;
}
