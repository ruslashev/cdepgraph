module GetFiles (getFileList) where

import System.Directory (getDirectoryContents, doesDirectoryExist)
import System.Exit (exitFailure)

getFileList :: String -> IO (Maybe ([String]))
getFileList dir = do
    dirValid <- doesDirectoryExist dir
    if dirValid then do
        fileList <- getAbsFileList dir
        return $ Just fileList
    else
        return Nothing
    -- doing nothing right now,
    -- TODO trim repeating prefixes in paths

getAbsFileList :: String -> IO ([String])
getAbsFileList dir = do
    folderContents <- (getDirectoryContents dir)

    let notHidden = filter (\ x -> head x /= '.') folderContents

    let thisDirectory = appendSlash dir

    let contentsAbsPath = map (thisDirectory ++) notHidden

    fileList <- openDirs contentsAbsPath

    return fileList

appendSlash dir = if last dir == '/' then dir else dir ++ "/"

openDirs :: [String] -> IO ([String])
openDirs [] = return []
openDirs (x:xs) = do
    isDirectory <- doesDirectoryExist x
    fileList <- if isDirectory then getAbsFileList x
                else return [x]
    rest <- openDirs xs
    return $ fileList ++ rest

