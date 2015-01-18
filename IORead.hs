module IORead (getFileList)
where

import Control.Applicative
import System.Directory (getDirectoryContents, doesDirectoryExist)

getFileList :: String -> IO (Either String [String])
getFileList dir = do
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
        abs <- (map ((dir++"/") ++) . filter (\x -> head x /= '.'))
                <$> getDirectoryContents dir
        concat <$> mapM getFiles abs

