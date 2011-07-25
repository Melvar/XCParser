
import Text.ParserCombinators.Parsec (parse)
import Extended (file, standardize)
import XCParser (unparse)

main :: IO ()
main = interact transform

transform :: String -> String
transform s = case parse file "Stdin" s of
                Left err -> show err ++ "\n"
                Right tree -> unparse $ standardize tree

