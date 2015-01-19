module Processing (process)
where

import Data.Char (toLower)
import qualified Data.Text as T
import qualified Data.Text.IO as Tio

data SrcFile = SrcFile { name :: String
                       , includes :: [String]
                       }

sourceFileExts, headerFileExts :: [String]
sourceFileExts = ["cpp", "c", "cxx", "cc", "cp", "c++"]
headerFileExts = ["hpp", "h", "hxx", "hh", "hp", "h++"]

isSourceFile :: String -> Bool
isSourceFile file =
    extension `elem` (sourceFileExts ++ headerFileExts)
    where extension = (map toLower . reverse . takeWhile (/= '.') . reverse) file

process :: [String] -> IO [String]
process files = do
    let srcs = filter isSourceFile files
    -- includes <- start srcs
    return srcs

-- start :: String -> IO SrcFile
-- start file = do
--     contents <- getIncludes <$> Tio.readFile file

getIncludes :: T.Text -> [T.Text]
getIncludes = map (T.tail . T.dropWhile (/= ' '))
            . filter ((includeText ==) . T.take (T.length includeText))
            . T.lines
    where includeText = T.pack "#include"

