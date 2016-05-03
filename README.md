#Quickterm [Quick Haskell Command Line Interface]

I often want to port certain functions I write to a command line interface. Assuming the existence of various functions which map from a set of command line arguments to an action of type IO (), this involes parsing arguments, options, and generating some usage information which should give you an idea of how to use the application if you go wrong.

I originally solved this with the idea of a Feature, which can be a Program with a list of features, or a Command, which contains a function [String] -> IO (). I did not include the concept of an option within this and I would like to handle this properly here. Both of the constructions of a Feature contained as well a name and a usage string, something which can recursively be generated and printed to the user upon failure to find a Command to run.

I want to step back from this and take a more principled view, seeing which useful Haskell typeclasses within which I can instantiate the various types I found useful in the last attempt.
