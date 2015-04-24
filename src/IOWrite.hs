module IOWrite
where

import qualified Data.Text as T

import Control.Applicative

import Processing.Clusters
import Processing.Includes

header :: [T.Text]
header = map T.pack
    [ "digraph G {"
    , "graph [overlap=prism,splines=true]"
    , "node [shape=box,style=filled,fontname=\"Sans\",fontsize=12.0];"
    ]

genOutput :: [Cluster] -> [SrcFileI] -> T.Text
genOutput clusters srcFilesI =
    let nodes = concatMap (textForCluster srcFilesI) clusters 
    in T.unlines $
        header ++
        nodes

textForCluster :: [SrcFileI] -> Cluster -> T.Text
textForCluster srcFiles (Folder name next) =
    T.intercalate (T.pack "\n") $
    [ T.pack "subgraph cluster_" `T.append` name `T.append` T.pack " {" ] ++
    [ T.pack "style=filled;" ] ++
    [ T.pack "color=lightgrey;" ] ++
    [ T.pack "label=" `T.append` name `T.append` T.pack ";" ] ++
    [textForCluster srcFiles next] ++
    [ T.pack "}" ]
textForCluster srcFiles (SystemInclude name) = textForNode name srcFiles
textForCluster srcFiles (File name) = textForNode name srcFiles

textForNode :: T.Text -> [SrcFileI] -> T.Text
textForNode name srcFiles =
    lookupT srcFiles name `T.append`
    T.pack " [label=\"" `T.append`
    name `T.append`
    T.pack "\", " `T.append`
    colorizeNode name `T.append`
    T.pack "]"

lookupT :: [T.Text] -> T.Text -> T.Text
lookupT hay needle = T.pack $ show $ lookupI hay needle

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

