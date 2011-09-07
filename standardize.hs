
import Text.ParserCombinators.Parsec (parse)
import Extended (file, standardize)
import XCParser (unparse)

import System.IO (hPutStrLn, stderr)
import System.Exit (ExitCode (..), exitWith)

main :: IO ()
main = do
    cont <- getContents
    case parse file "stdin" cont of
        Left err -> do
            putStrLn $ show err
            exitWith $ ExitFailure 1
        Right tree -> do
            let (errs,goods) = standardize tree
            mapM_ (hPutStrLn stderr) errs
            putStr $ unparse goods
            exitWith (if null errs
                      then ExitSuccess
                      else ExitFailure 2)

