module Processing.Includes
    ( processIncludes
    , SrcFile(..)
    , getExtension
    , sourceFileExts
    , headerFileExts
    )
where

-- Processing.Includes, produces list of files and their includes

import Control.Applicative ((<$>))
import Data.Char (toLower)
import qualified Data.Text as T
import qualified Data.Text.IO as Tio -- Data.Text.Lazy.IO

data SrcFile = SrcFile { name :: T.Text
                       , includes :: [T.Text]
                       } deriving (Show)

processIncludes :: [String] -> IO [SrcFile]
processIncludes = mapM getSrcFile . filter isSourceFile . map T.pack

isSourceFile :: T.Text -> Bool
isSourceFile file =
    getExtension file `elem` sourceFileExts ||
    getExtension file `elem` headerFileExts

getExtension :: T.Text -> T.Text
getExtension = T.map toLower . T.reverse . T.takeWhile (/= '.') . T.reverse

sourceFileExts, headerFileExts :: [T.Text]
sourceFileExts = map T.pack ["cpp", "c", "cxx", "cc", "cp", "c++"]
headerFileExts = map T.pack ["hpp", "h", "hxx", "hh", "hp", "h++"]

getSrcFile :: T.Text -> IO SrcFile
getSrcFile filepath = do
    absIncludes <- map (convToAbs filepath) . getIncludes <$>
        Tio.readFile (T.unpack filepath)
    return $ SrcFile filepath absIncludes

getIncludes :: T.Text -> [T.Text]
getIncludes = map (T.drop 1 . T.dropWhile (/= ' '))
            . filter ((includeText ==)
            . T.take (T.length includeText))
            . T.lines
    where includeText = T.pack "#include"

convToAbs :: T.Text -> T.Text -> T.Text
convToAbs filepath include =
    if x == '"' then
        let fileDir = getDirectory filepath in
        if T.take 3 xs == T.pack "../" then
            let upperDirectory = getDirectory fileDir
            in convToAbs (upperDirectory `T.append` T.pack "/")
                            (T.pack "\"" `T.append` T.drop 3 xs)
        else
            if T.take 2 xs == T.pack "./" then
                convToAbs filepath (T.pack "\"" `T.append` T.drop 3 xs)
            else
                fileDir `T.append` T.cons '/' (T.init xs)
    else -- angular brackets or #defined path
        include
    where x  = T.head include
          xs = T.tail include

getDirectory :: T.Text -> T.Text
getDirectory = T.reverse . T.drop 1 . T.dropWhile (/= '/') . T.reverse

