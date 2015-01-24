module IOWrite (genOutput)
where

import Processing (SrcFile, getExtension)
import qualified Data.Text as T

header :: [T.Text]
header =
    [ T.pack "digraph G"
    , T.pack "{"
    , T.pack "graph [splines=true, overlap=scale]"
    , T.pack "node [shape=box, style=filled, fontname=\"Sans\", fontsize=12.0];"
    ]

genOutput :: [SrcFile] -> T.Text
genOutput srcFiles = T.unlines $ header ++ assignNodes srcFiles

assignNodes :: [SrcFile] -> [T.Text]

colorizeNode :: String -> String
colorizeNode inc
    | head inc == '<'                 = blue
    | ext `elem` sourceFileExtensions = yellow
    | ext `elem` headerFileExtensions = green
    | otherwise                       = red
    where ext    = getExtension inc
          blue   = "color=\"#D5EEFB\""
          yellow = "color=\"#F8F8D3\""
          green  = "color=\"#D4F9D4\""
          red    = "color=\"#FAD5D5\""

