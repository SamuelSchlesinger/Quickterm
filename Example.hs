import Quickterm
import System.Environment

quickterm = Choice "quickterm" [ Command "create" (\args -> \opts -> putStrLn ((show args) ++ ", " ++ (show opts))) "" ] "create" 

main = do
    args <- getArgs
    quickrun args quickterm
