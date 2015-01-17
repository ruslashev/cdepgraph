module IORead (getFileList)
where

import System.Directory (getDirectoryContents, doesDirectoryExist)

getFileList :: String -> IO (Either String [String])
getFileList dir = do
    valid <- doesDirectoryExist dir
    if valid then do
        list <- getFiles dir
        return $ Right list
    else return $ Left $ "Directory \"" ++ dir ++ "\" doesn't exist"

toAbsolutePath :: String -> [String] -> [String]
toAbsolutePath dir = map (dir ++)

-- foldl calling
getFiles :: String -> IO [String]
getFiles dir = do
    isDir <- doesDirectoryExist dir
    if not isDir then
        return [dir]
    else do
        contents <- getDirectoryContents dir
        let abs = toAbsolutePath (appendSlash dir) $ filter (\x -> head x /= '.') contents
        nested <- mapM getFiles abs
        return $ concat nested

appendSlash :: String -> String
appendSlash dir = if last dir == '/' then dir else dir ++ "/"

