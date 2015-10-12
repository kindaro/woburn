{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Woburn.Protocol
where

import Graphics.Wayland
import Language.Haskell.TH (runIO)
import Woburn.Path

$(runIO waylandXmlPath >>= generateFromXml Server)
