module Woburn.Path
    ( waylandXmlPath
    )
where

import Control.Applicative
import Prelude
import System.Process

-- | Uses pkg-config to find the path of the wayland protocol specification.
waylandXmlPath :: IO String
waylandXmlPath =
    (++ "/wayland.xml") . head . lines
    <$> readProcess "pkg-config" ["--variable=pkgdatadir", "wayland-server"] ""
