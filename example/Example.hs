import Quickterm
import System.Directory
import System.Environment
import Data.HashMap
import Prelude hiding (lookup)
import Data.List hiding (lookup)

features = [ Command "read" qtReadFile "<filename> -- reads file out"
          ,  Command "copy" qtCopyFile "<filename> <newfilename> -- copies file" 
          ,  Command "write" qtWriteFile "[--space <space>] <filename> {<towrite>} -- writes to file"
          ,  Command "home?" qtPrintHomeDirectory "-- prints users home directory" ] 

example = Choice "Example" features ""

main = do
    args <- getArgs
    quickrun args example

-- Reads a file to the command line
qtReadFile :: TerminalAction
qtReadFile (filename:_) _ = do
    contents <- readFile filename
    putStrLn contents
qtReadFile _ _ = putStr $ usage example

-- Copies a file from filename to filename'
qtCopyFile :: TerminalAction
qtCopyFile (filename:filename':_) _ = do
    contents <- readFile filename
    writeFile filename' contents
qtCopyFile _ _ = putStr $ usage example

-- Writes the rest of the arguments to filename, separated by whatever
-- is specified by the option -s <space>. Is " " by default.
qtWriteFile :: TerminalAction
qtWriteFile (filename:rest) opts = do
    case lookup "space" opts of
        Nothing -> writeFile filename (intercalate " " rest)
        Just (space:_) -> writeFile filename (intercalate space rest)

-- Prints the users home directory
qtPrintHomeDirectory :: TerminalAction
qtPrintHomeDirectory _ _ = do
    homedir <- getHomeDirectory
    putStrLn homedir
