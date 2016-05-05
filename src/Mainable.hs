import Control.Monad
import System.Environment

-- | There was a very small class of functions which
--   were allowed in the previous round of this code,
--   specifically the space of TerminalActions as
--   defined by: 
-- 
--   type TerminalAction = Args -> Options -> IO ()
--   
--   where type (Args, Options) = ([String], Map String [String])
--
--   Basically the logic here is that the actual command
--   line can only pass string arguments. We only retrieve [String]
--   (well, IO [String]), from getArgs, but what am I really
--   ensuring when I make these type restrictions?
--
--   Well, I was trying to ensure that the functions which were
--   considered available to Quickterm programs were able to
--   get the representations that they need, and as [String] is
--   the type of thing we're getting, naively that would be the
--   only type we should allow, and we can obviously just convert
--   from there within the function.
--
--   Within the function, how might this look? Well, let's pretend
--   that anybody who wanted to use Quickterm generated a Read
--   instance for any of the variables that are being passed
--   to their functions. Then, the operation of their program would
--   generically look something like this:
--   
--   myProg (arg1:arg2:arg3:...) = do
--       let x = read arg1 :: T1
--       let y = read arg2 :: T2
--       let z = read arg3 :: T3
--       ... (arbitrary stuff)
--    
--   This would suggest that there is a generalization of TerminalAction
--   the type in a class which I will call Mainable, which we can define
--   in the following way.

-- | The logic goes something like this: A Mainable type is one that
--   can be turned into a function from [String] -> IO.
class Mainable m where
    mainify :: m -> [String] -> IO ()

-- | Thus, (Show a) is Mainable as we simply print it out.
instance Mainable (IO a) where
    mainify main _ = void main

-- | This is the inductive step.
instance (Read r, Mainable m) => Mainable (r -> m) where
    mainify main (arg:args) = mainify (main (read arg)) args
