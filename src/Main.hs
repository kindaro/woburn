module Main
where

import Control.Concurrent.MChan.Split
import Woburn.Backend.Gtk
import Woburn.Core

main :: IO ()
main = do
    (clientRd, clientWr)    <- newMChan
    (reqWr, evtRd, surfGet) <- gtkBackend
    closeMChan clientWr
    run reqWr evtRd surfGet clientRd
