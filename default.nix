{ mkDerivation, base, containers, diet-set, linear, mtl, QuickCheck, pipes, pipes-concurrency, async
, stdenv, wayland-wire, vim, cabal-install, wayland, pkgconfig, hlint
, git, less
}:
mkDerivation {
  pname = "woburn";
  version = "0.1.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildTools = [ vim cabal-install pkgconfig hlint git less ];
  buildDepends = [
    base containers diet-set linear mtl wayland-wire wayland pipes pipes-concurrency async
  ];
  testDepends = [
    base containers diet-set linear mtl QuickCheck wayland-wire pipes pipes-concurrency async
  ];
  description = "Wayland compositor";
  license = stdenv.lib.licenses.gpl3;
}
