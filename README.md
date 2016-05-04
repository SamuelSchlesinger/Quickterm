#Quickterm [Quick Haskell Terminal Interfaces]

##Install/Documentation

To install:

> cabal install quickterm

Documentation: https://hackage.haskell.org/package/quickterm

##First Look (Thoughts)

I often want to port certain functions I write to a command line interface. Assuming the existence of various functions which map from a set of command line arguments to an action of type IO (), this involes parsing arguments, options, and generating some usage information which should give you an idea of how to use the application if you go wrong.

I originally solved this with the idea of a Feature, which could be a Program with a list of features, or a Command, which contains a function [String] -> IO (). I did not include the concept of an option within this and I would like to handle this properly here. Both of the constructions of a Feature contained as well a name and a usage string, something which can recursively be generated and printed to the user upon failure to find a Command to run.

I want to step back from this and take a more principled view, seeing which useful Haskell typeclasses within which I can instantiate the various types I found useful in the last attempt. (Looking back, basically none were needed or would have added much as far as I can see. Living in the IO Monad is basically enough.) 

##Second Look (First Interface)

importing the Quickterm module will give you the following definitions:

> type Name = String -- I like this better than saying String everywhere, more meaning.

> type Usage = String -- I like to describe things with characters, ideally with words.

> type Args = [String] -- Nothing special here

> type Options = Map Name [String] -- The options are a mapping from Name -> [String]
> -- It's rather important to note that an option of the form -X takes no argument,
> -- an option of the form --X can take one, ---X two, and so on.

> type TerminalAction = Args -> Options -> IO () -- The sort of action we can perform

> data Quickterm = Choice Name [Quickterm] Usage
               | Command Name TerminalAction Usage 

> usage :: Quickterm -> String -- Generates the usage string

> quickrun :: [String] -> Quickterm -> IO () -- Runs the program on the raw arguments

