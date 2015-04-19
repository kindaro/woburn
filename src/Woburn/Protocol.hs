{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Woburn.Protocol
where

import Graphics.Wayland

$(generateFromXml Server "/usr/share/wayland/wayland.xml")
