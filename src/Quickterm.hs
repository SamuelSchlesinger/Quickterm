module Quickterm
    ( Quickterm(..)
    , Options
    , Name
    , Args
    , usage
    , TerminalAction
    , quickrun
    ) where

import Text.EditDistance
import qualified Data.HashMap as M

-- | I tend to call my applications by some name
--
type Name = String

-- | With each of my endpoints for computation, I like to have some
--   description of their usage.
type Usage = String

-- | The sorts of actions which take place on the terminal are of
--   the form "cmd {<arg> | ("--" | "-") <opt-name> <opt-val>}".
--   The arguments are, in general, sequentially ordered.
type Args = [String]

-- | The options, on the other hand, tend to be a mapping from some
--   set of names to some string.
type Options = M.Map Name [String]

-- | The terminal action itself should take some arguments, some options,
--   and be able to produce some IO action dependent based on these.
--   The key here is that the TerminalAction is a pure function, taking
--   pure values and deterministically attempting some IO action based on
--   these. This is important to me in terms of designing and using programs.
type TerminalAction = Args -> Options -> IO ()

-- | In general, the applications I use have two sorts of endpoints:
--
--   1) The application has a main command which takes arguments and options
--      and performs basically the same behavior, with some variation, based
--      on these inputs.
--
--   2) The application has a variety of sub-modules which have their own pretty
--      unique functionality and usage. I prefer these for two reasons: The first
--      is that, as the user, it separates the learning curve for one aspect of
--      the program from some other one. The second is that it allows me as the
--      developer of such software to truly make modular systems and plug them in
--      and unplug them as I wish.
--
--   As I like this and I am developing this, I will then focus my efforts as such.
--   We shall call a program which contain submodules a Choice, whereas we shall
--   call a program which is simply a command to be run a Command.
--
--   A Choice should simply have a Name, a list of Quickterms, and
--   a description of its Usage.
--
--   A Command may simply have a Name, a TerminalAction, and a description
--   of its Usage.
data Quickterm = Choice Name [Quickterm] Usage
               | Command Name TerminalAction Usage

--   Gets the name from an arbitrary Quickterm
--
qtName :: Quickterm -> String
qtName (Choice name _ _) = name
qtName (Command name _ _) = name

-- | This function will generate the usage information and will be exposed at the top level.
usage x = "usage: \n" ++ usage' 0 x

--   This function will gather the usage information, indented properly for the given
--   depth.
--
usage' :: Int -> Quickterm  -> String
usage' n (Choice name branches use) = replicate n ' ' ++ name ++ " " ++ use ++ "\n"
                                    ++ concatMap (usage' (n + 4)) branches
usage' n (Command name _ use) = replicate n ' ' ++ name ++ " " ++ use ++ "\n"

-- | This is a function which will take a top level program and attempt to run it,
--   printing out the usage information for the deepest Choice achieved if the attempt
--   is failed, i.e we did not find a Command to run at all.
quickrun :: [String] -> Quickterm -> IO ()
quickrun x = quickrun' args opts where
    (args, opts) = organizeInput x
    quickrun' [] _ (Choice _ _ use) = putStr use -- No way to make a choice without input
    quickrun' args opts (Command _ action _) = action args opts
    quickrun' (nextbranch:args) opts choice@(Choice name branches use) = case findbranch nextbranch branches of
                                                         Nothing -> whoopsy nextbranch
                                                                           (usage choice)
                                                                            branches
                                                         Just branch -> quickrun' args opts branch

--   Finds a branch, given a name, in a list of Quickterms
--
findbranch :: String -> [Quickterm] -> Maybe Quickterm
findbranch _ [] = Nothing
findbranch name ((choice@(Choice name' _ _)) : next) = if name == name' then Just choice else findbranch name next
findbranch name ((command@(Command name' _ _)) : next) = if name == name' then Just command else findbranch name next

--   Finds the nearest branch, if there is one, and prints out "Did you mean <branch>?",
--   followed by the usage, which should be passed as the second argument.
whoopsy :: String -> String -> [Quickterm] -> IO ()
whoopsy name use list = case findnearestbranch name list of
  Just branch -> putStrLn ("Did you mean " ++ qtName branch ++ "?")
  _           -> putStr use

--   Finds the branch which most nearly matches the name given,
--   starting the lowest edit distance at 100000 cause if you
--   make your arguments long enough to get edit distances that high
--   you're wrong.
--
findnearestbranch :: String -> [Quickterm] -> Maybe Quickterm
findnearestbranch name = findnearestbranch' name Nothing 3 where
    findnearestbranch' :: String -> Maybe Quickterm -> Int -> [Quickterm] -> Maybe Quickterm
    findnearestbranch' _ mqt _ [] = mqt
    findnearestbranch' name nearest distance (branch:xs) = if distance > distance'
        then findnearestbranch' name (Just branch) distance' xs
        else findnearestbranch' name nearest distance xs where
            distance' = levenshteinDistance defaultEditCosts name (qtName branch)

--   Options are of the form {-}<opt-name> {<arg>}, where the number of arguments to
--   the option is the number of dashes minus one.
--
--   The role of the rather nasty function below will be to extract the Options from
--   the list of strings and return the list of strings.
--
organizeInput :: [String] -> (Args, Options)
organizeInput x = (args, opts) where
    (args, opts) = organizeInput' x [] M.empty
    organizeInput' :: [String] -> Args -> Options -> (Args, Options)
    organizeInput' [] args opts = (reverse args, opts)
    organizeInput' (('-':restopt):xs) args opts = organizeInput' rest args
                                                                (M.insert name vals opts) where
        (name, rest, vals) = getOpt restopt xs [] 0
        getOpt :: String -> [String] -> [String] -> Int -> (String, [String], [String])
        getOpt ('-':more) xs args n = getOpt more xs args (n + 1)
        getOpt name xs args n = (name, rest, vals) where
            (vals, rest) = splitAt n xs
    organizeInput' (x:xs) args opts = organizeInput' xs (x:args) opts
