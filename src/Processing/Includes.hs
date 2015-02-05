module Processing.Includes
    ( process
    , SrcFile(..)
    , getExtensionT
    , sourceFileExtsT
    , headerFileExtsT
    )
where

import Control.Applicative
import Data.Char (toLower)
import qualified Data.Text as T
import qualified Data.Text.IO as Tio

data SrcFile = SrcFile { name :: T.Text
                       , includes :: [T.Text]
                       } deriving (Show)

process :: [String] -> IO [SrcFile]
process = mapM getSrcFile . filter isSourceFile

getExtension :: String -> String
getExtension = map toLower . reverse . takeWhile (/= '.') . reverse

getExtensionT :: T.Text -> T.Text
getExtensionT = T.map toLower . T.reverse . T.takeWhile (/= '.') . T.reverse

getDirectory :: T.Text -> T.Text
getDirectory = T.reverse . T.drop 1 . T.dropWhile (/= '/') . T.reverse

sourceFileExts, headerFileExts :: [String]
sourceFileExts = ["cpp", "c", "cxx", "cc", "cp", "c++"]
headerFileExts = ["hpp", "h", "hxx", "hh", "hp", "h++"]
sourceFileExtsT, headerFileExtsT :: [T.Text]
sourceFileExtsT = map T.pack sourceFileExts
headerFileExtsT = map T.pack headerFileExts

isSourceFile :: String -> Bool
isSourceFile file =
    getExtension file `elem` (sourceFileExts ++ headerFileExts)

getSrcFile :: String -> IO SrcFile
getSrcFile file = do
    let fileAsText = T.pack file
    absIncludes <- map (convToAbs fileAsText) . getIncludes <$> Tio.readFile file
    return $ SrcFile fileAsText absIncludes

getIncludes :: T.Text -> [T.Text]
getIncludes = map (T.drop 1 . T.dropWhile (/= ' '))
            . filter ((includeText ==) . T.take (T.length includeText))
            . T.lines
    where includeText = T.pack "#include"

convToAbs :: T.Text -> T.Text -> T.Text
convToAbs file include =
    if x == '"' then
        let fileDir = getDirectory file in
        if T.take 3 xs == T.pack "../" then
            let upperDirectory = getDirectory fileDir
            in convToAbs (upperDirectory `T.append` T.pack "/")
                            (T.pack "\"" `T.append` T.drop 3 xs)
        else
            if T.take 2 xs == T.pack "./" then
                convToAbs file (T.pack "\"" `T.append` T.drop 3 xs)
            else
                fileDir `T.append` T.cons '/' (T.init xs)
    else -- angular brackets or #defined path
        include
    where x  = T.head include
          xs = T.tail include
