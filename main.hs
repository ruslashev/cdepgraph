import System.Environment (getArgs)

source = unlines [
    "digraph G {",
    "node [shape=box/*,fixedsize=true,width=2.0*/]; a; b; c;",
    "a -> b;",
    "b -> c;",
    "overlap=false",
    "}"]

main = do
    args <- getArgs
    mapM_ putStrLn args
    putStr source

