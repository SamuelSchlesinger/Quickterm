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
--   can be turned into a function from strings.
class Mainable m where
    run :: m -> [String] -> IO ()

-- | Thus, IO () is Mainable as we simply ignore the String and call
--   main.
instance Mainable (IO a) where
    run main _ = void main

-- | As well, a function from String -> IO () is clearly Mainable,
--   as we can simply call this function on the string.
-- instance Mainable (String -> IO ()) where
--    runmain main args = main args

-- | This is the inductive step, and it basically says this:
--   If we have a Mainable type m, and something we can Read,
--   then we can make a function from String simply by composing
--   our main function with read.
instance (Read r, Mainable m) => Mainable (r -> m) where
    run main (arg:args) = run (main (read arg)) args

-- | This compiles fine and I personally feel like it does what I want,
--   but the compiler constantly complains when I actually try to use it.

test :: Integer -> Integer -> IO ()

test a b = putStrLn $ show (a + b)

main = do
    args <- getArgs
    run test args
