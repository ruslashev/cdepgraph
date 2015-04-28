module IORead (getAbsFileList)
where

-- IORead, gets list of files in directories

import Control.Applicative
import System.Directory (getDirectoryContents, doesDirectoryExist)

getAbsFileList :: String -> IO (Either String [String])
getAbsFileList dir = do
    valid <- doesDirectoryExist dir
    if valid then do
        list <- getFiles dir
        return $ Right list
    else return $ Left $ "Directory \"" ++ dir ++ "\" doesn't exist"

getFiles :: String -> IO [String]
getFiles dir = do
    isDir <- doesDirectoryExist dir
    if not isDir then
        return [dir]
    else do
        -- abs <- (map (ensureSlash . (++) dir) . filter (\x -> head x /= '.'))
        abs <- (map (ensureSlash dir ++) . filter (\x -> head x /= '.'))
                <$> getDirectoryContents dir
        concat <$> mapM getFiles abs

ensureSlash :: String -> String
ensureSlash dir = if last dir == '/' then dir else dir ++ "/"

