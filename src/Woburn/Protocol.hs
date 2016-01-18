{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Woburn.Protocol
where

import Graphics.Wayland
import Woburn.Path

$(generateFromXml Server waylandXmlPath)
