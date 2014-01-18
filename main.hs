import System.Environment (getArgs, getProgName)
import System.Directory (getDirectoryContents, doesDirectoryExist)
import Control.Monad (forM_)

source = unlines [
    "digraph G {",
    "\tnode [shape=box/*,fixedsize=true,width=2.0*/];",
    "\toverlap=false"
    ]

main = do
    args <- getArgs
    -- if length args /= 1 then do
    --     progName <- getProgName
    --     putStr $ unlines [
    --           "Usage: " ++ progName ++ " <directory>"
    --         , "Start a scan for source files in specified directory"
    --         ]
    -- else
    startScan $ "link-to-dir" -- head args

startScan :: FilePath -> IO ()
startScan dir = do
    files <- getFileList dir
    let srcFiles = filter (\ x -> dropWhile (/= '.') x `elem`
                     [".cpp", ".c", ".cxx", ".cc", ".h", ".hh", ".hpp", ".hxx"])
                     files
    strippedFiles <- mapM getPPdirectives srcFiles
    let filesAndTheirIncludes = zip srcFiles $ map scanFile strippedFiles
    putStrLn source
    forM_ filesAndTheirIncludes (\ (file,incs) -> do
            forM_ incs (\ inc ->
                let unrelFile = drop (length dir + 1) file
                in putStrLn $ "\t" ++ show unrelFile ++ " -> " ++ show inc
                )
        )
    putStrLn "}"

scanFile :: [String] -> [String]
scanFile [] = []
scanFile (x:xs) =
    let (kind,arg) = break (== ' ') x in
        case kind of "#include" -> tail arg : scanFile xs
                     _          -> scanFile xs

getPPdirectives :: FilePath -> IO ([String])
getPPdirectives file = do
    fileContents <- readFile file
    return $ filter (\ x -> not (null x) && head x == '#') $ lines fileContents

getFileList :: FilePath -> IO ([String])
getFileList dir = do
    folderContents <- getDirectoryContents dir
    let relativeFolderContents =
            map ((dir ++ "/") ++) $ filter (\ x -> head x /= '.') folderContents
    fileList <- getFiles dir relativeFolderContents
    return fileList
    where
    getFiles :: FilePath -> [FilePath] -> IO ([String])
    getFiles dir (x:xs) = do
        isDir <- doesDirectoryExist x
        if isDir then do
            files <- getFileList x
            rest  <- getFiles dir xs
            return $ files ++ rest
        else do
            rest <- getFiles dir xs
            return $ x : rest
    getFiles _ [] = return []

