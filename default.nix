{ cabal, cabalInstall_1_20_0_6, filterSource, QuickCheck, waylandWire }:

cabal.mkDerivation
( self:
  { pname = "woburn"
  ; version = "0.1.0"
  ; src = filterSource ./.
  ; buildTools = [ cabalInstall_1_20_0_6 ]
  ; buildDepends = [ waylandWire ]
  ; testDepends = [ QuickCheck ]
  ; doCheck = true
  ; enableSplitObjs = false
  ;
  }
)
