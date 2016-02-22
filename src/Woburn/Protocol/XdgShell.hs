{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Woburn.Protocol.XdgShell
where

import Graphics.Wayland
import Woburn.Path
import Woburn.Protocol.Core

$(generateFromXml Server (xmlPath "xdg-shell-unstable-v5.xml"))
