module IOWrite
where

import qualified Data.Text as T

import Processing.Clusters

header :: [T.Text]
header = map T.pack
    [ "digraph G {"
    , "graph [overlap=prism,splines=true]"
    , "node [shape=box,style=filled,fontname=\"Sans\",fontsize=12.0];"
    ]

genOutput :: [Cluster] -> [SrcFileI] -> T.Text
genOutput clusters srcFiles =
    let nodes = assignNodes clusters srcFilesI
    in T.unlines $
        header

textForNode :: T.Text -> [SrcFilesI] -> [T.Text]
textForNode name srcFiles =
    lookupT name srcFiles `T.append`
    T.pack " [label=\"" `T.append`
    name `T.append`
    T.pack "\", " `T.append`
    colorizeNode name `T.append`
    T.pack "]"

lookupT = T.pack . show . lookupI

colorizeNode :: T.Text -> T.Text
colorizeNode text
    | T.head text == '<'         = blue
    | ext `elem` sourceFileExtsT = yellow
    | ext `elem` headerFileExtsT = green
    | otherwise                  = red
    where ext = getExtensionT text
          blue   = T.pack "color=\"#D5EEFB\""
          yellow = T.pack "color=\"#F8F8D3\""
          green  = T.pack "color=\"#D4F9D4\""
          red    = T.pack "color=\"#FAD5D5\""

