module Woburn.Path
    ( waylandXmlPath
    , xmlPath
    )
where

import System.FilePath

xmlPath :: String -> String
xmlPath = ("protocols" </>)

waylandXmlPath :: String
waylandXmlPath = xmlPath "wayland.xml"
