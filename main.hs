import System.Environment (getArgs, getProgName)

source = unlines [
    "digraph G {",
    "node [shape=box/*,fixedsize=true,width=2.0*/]; a; b; c;",
    "a -> b;",
    "b -> c;",
    "overlap=false",
    "}"]

main = do
    args <- getArgs
    progName <- getProgName
    if length args /= 1 then
        putStr $ unlines [
              "Usage:"
            , progName ++ " (directory)"
            , ""
            , "Start a scan for source files in specified directory"
            ]
    else
        startScan $ head args

startScan :: String -> IO ()
startScan dir = do
    putStrLn "yeah"

