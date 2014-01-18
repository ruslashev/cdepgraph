import System.Environment (getArgs, getProgName)
import System.Directory (getDirectoryContents, doesDirectoryExist)
import Control.Monad (forM_)

source = unlines [
    "digraph G {",
    "\tgraph [splines=true,overlap=scale]",
    "\tnode [shape=box,fontname=\"Sans\",fontsize=12.0];"
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
startScan directory = do
    let dir = reverse $ dropWhile (== '/') $ reverse directory

    files <- getFileList dir
    let srcFilesList = filter (\ x -> dropWhile (/= '.') x `elem`
                     [".cpp", ".c", ".cxx", ".cc", ".h", ".hh", ".hpp", ".hxx"])
                     files

    putStrLn source

    forM_ srcFilesList (\ file -> do
            stripped <- getPPdirectives file
            let includes = scanForIncludes stripped
            relIncludes <- getRelIncludes file includes
            forM_ relIncludes (\ inc -> do
                putStrLn $ "\t" ++ show file ++ " -> " ++ show inc
                )
            )

    putStrLn "}"

getRelIncludes :: String -> [String] -> IO [String]
getRelIncludes _ [] = return []
getRelIncludes file incs@(x:xs) = do
    path <- scanInclude file x
    rest <- getRelIncludes file xs
    return $ path : rest

scanInclude :: String -> String -> IO String
scanInclude file inc@(x:xs) = do
    if x == '<' then do
        return inc
    else
        let brokenPath = break (== '/') $ reverse file
            path = reverse $ drop 1 $ snd brokenPath
            fileName = reverse $ fst brokenPath
        in
        if take 2 xs == "./" then do
            inc <- scanInclude file ("\"" ++ drop 3 xs)
            return inc
        else
            if take 3 xs == "../" then
                let backDir = reverse $ drop 1 $ dropWhile (/= '/') $ reverse path
                in do
                    scanInclude (backDir ++ "/" ++ fileName) ("\"" ++ drop 3 xs)
            else
                if x == '"' then do
                    return $ path ++ "/" ++ init xs
                else do -- #defined path
                    return inc

scanForIncludes :: [String] -> [String]
scanForIncludes [] = []
scanForIncludes (x:xs) =
    let (kind,arg) = break (== ' ') x in
        case kind of "#include" -> tail arg : scanForIncludes xs
                     _          -> scanForIncludes xs

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

