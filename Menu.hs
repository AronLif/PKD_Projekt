import Control.Exception
import Prelude hiding(catch)

type Option = String

{- functionIdentifier arguments
     A brief human-readable description of the purpose of the function.
     PRE:  ... precondition on the arguments, if any ...
     RETURNS: ... description of the result, in terms of the arguments ...
     SIDE EFFECTS: ... side effects, if any, including exceptions ...
     EXAMPLES: ... especially if useful to highlight delicate issues; also consider including counter-examples ...
-}
{-
Imperative programming is a programming paradigm that describes computation in terms of
statements that change a program state [Wikipedia].
Imperative programming is programming with side-effects.

The type IO a indicates that an I/O action occurs, and that the resulting value has type a.
(() is a type that has only a single value ().)
I/O actions are treated differently in Haskell.-}

{- readOption
    Auxiliary function for firstBase and secondBase. Reads a written value in the terminal which
    is requested by either firstBase or secondBase.
    RETURNS: Written value as String
    EXAMPLES: 
-}
readOption :: IO Option
readOption = do
    catch (do
        line <- getLine
        evaluate (read line))
        ((\_ -> do
            putStrLn "Invalid input"
            readOption) :: SomeException -> IO Option)

firstBase :: IO ()
firstBase = do
    putStrLn "\nRTX-Graphicscard\nChoose series:\n  3060\n  3070\n  3080"
    choice <- readOption
    if choice == "3060" || choice == "3070" || choice == "3080" then do
        secondBase choice
    else do
        putStrLn "Misspelled"
        firstBase

secondBase :: String -> IO ()
secondBase string = do
    putStrLn "\nChoose Brand:\n  Asus\n  MSI\n  Zotac"
    choice <- readOption
    if choice == "Asus" || choice == "MSI" || choice == "Zotac" then do
        putStrLn (mergeText string choice)
    else do
        putStrLn "Misspelled"
        secondBase string

mergeText :: Option -> Option -> Option
mergeText string1 string2 = string1 ++ " " ++ string2