module Woburn.Path
    ( waylandXmlPath
    )
where

import System.FilePath

xmlPath :: String -> String
xmlPath = ("protocols" </>)

waylandXmlPath :: String
waylandXmlPath = xmlPath "wayland.xml"
