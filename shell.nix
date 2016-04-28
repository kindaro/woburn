{ profiling ? false }:
let
  orignalPackages = (import <nixpkgs>{}).haskellPackages;
  haskellPackages =
  if profiling
  then orignalPackages.override {
    overrides = self : super: { mkDerivation = drv : super.mkDerivation (drv // {enableLibraryProfiling = true;});
    };
  }
  else orignalPackages;
  all = (import ../../nix-packages.nix) { haskellPackages = haskellPackages; isShell = true; };
in
all.woburn.env
