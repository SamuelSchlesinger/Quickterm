module Quickterm where

import Data.HashMap hiding (filter)

-- | I tend to call my applications by some name
--
type Name = String

-- | With each of my endpoints for computation, I like to have some
--   description of their usage. 
--
type Usage = String

-- | The sorts of actions which take place on the terminal are of
--   the form "cmd {<arg> | ("--" | "-") <opt-name> <opt-val>}".
--   The arguments are, in general, sequentially ordered.
--
type Args = [String]

-- | The options, on the other hand, tend to be a mapping from some
--   set of names to some string.
--
type Options = Map Name String

-- | The terminal action itself should take some arguments, some options,
--   and be able to produce some IO action dependent based on these.
--   The key here is that the TerminalAction is a pure function, taking
--   pure values and deterministically attempting some IO action based on
--   these. This is important to me in terms of designing and using programs. 
--
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
--  
data Quickterm = Choice Name [Quickterm] Usage
               | Command Name TerminalAction Usage

instance Eq Quickterm where -- No, this is not in any sense extensional equality.
    (Choice _ _ _) == (Command _ _ _) = False
    (Choice name _ _) == (Choice name' _ _) = name == name'
    (Command name _ _) == (Command name' _ _) = name == name'

-- | This function will gather the usage information, indented properly for the given
--   depth.
-- 
usage' :: Quickterm -> Int -> String

-- | This is a function which will take a top level program and attempt to run it,
--   printing out the usage information for the deepest Choice achieved if the attempt 
--   is failed, i.e we did not find a Command to run at all.
--   
quickrun :: [String] -> Quickterm -> IO ()
quickrun x q = quickrun' args opts q 0 where
    (args, opts) = organizeInput x
    quickrun' args opts (Command _ action _) _ = action args opts

-- | I will assume options to be of the form --<name> <value>, or
--   -<name>.
--
--   The role of the rather nasty function below will be to extract the Options from
--   the list of strings and return the list of strings.
--   
organizeInput :: [String] -> (Args, Options)
organizeInput x = (args, opts) where
    organizeInput' :: [String] -> Args -> Options -> (Args, Options)
    organizeInput' [] args opts = (args, opts)
    organizeInput' (('-':'-':opt):[]) args opts = organizeInput' []
                                                                 args
                                                                 (insert opt "" opts)
    organizeInput' (('-':'-':opt):xs) args opts = organizeInput' (tail xs)
                                                                 args 
                                                                (insert opt (head xs) opts)
    organizeInput' (('-':opt):xs) args opts = organizeInput' xs 
                                                             args 
                                                            (insert opt "" opts)
    organizeInput' (x:xs) args opts = organizeInput' xs
                                                    (x:args)
                                                     opts     
    (args, opts) = organizeInput' x [] empty
