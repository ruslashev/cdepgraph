module IOWrite (genOutput)
where

import qualified Data.Text as T

import Processing

header :: [T.Text]
header =
    [ T.pack "digraph G"
    , T.pack "{"
    , T.pack "graph [splines=true, overlap=scale]"
    , T.pack "node [shape=box, style=filled, fontname=\"Sans\", fontsize=12.0];"
    ]

genOutput :: [SrcFile] -> T.Text
genOutput srcFiles =
    T.unlines $
        header ++
        assignNodes srcFiles

assignNodes :: [SrcFile] -> [T.Text]
assignNodes list =
    foldr
    (\ (idx, fileName) output ->
        T.pack (show idx ++ " [label=\"") `T.append`
        fileName `T.append`
        T.pack "\", " `T.append`
        colorizeNode fileName `T.append`
        T.pack "]"
        : output
    ) [] (zip [1..] listOfFilenames)
    where listOfFilenames = [ name srcFile | srcFile <- list ]

colorizeNode :: T.Text -> T.Text
colorizeNode file
    | ext `elem` sourceFileExts = yellow
    | ext `elem` headerFileExts = green
    where ext    = getExtension $ T.unpack file
          blue   = T.pack "color=\"#D5EEFB\""
          yellow = T.pack "color=\"#F8F8D3\""
          green  = T.pack "color=\"#D4F9D4\""
          red    = T.pack "color=\"#FAD5D5\""

