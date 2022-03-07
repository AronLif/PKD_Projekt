module Menu where
import Control.Exception ( SomeException, catch, evaluate )
import Prelude hiding(catch)
import Crawler ( crawler )

{- Option
    Represents selected menu option
-}
type Option = String

{- start
    An eysier command to start the program
    RETURNS: Selected products
    SIDE EFFECTS: Can not terminate untill all options have been selected
    EXAMPLES: start == firstBase
-}
start :: IO ()
start = do firstBase

{- readOption
    Auxiliary function for firstBase and secondBase. Reads a written value in the terminal which
    is requested by either firstBase or secondBase.
    RETURNS: Written value as String
    SIDE EFFECT: Can not terminate until a string is received
    EXAMPLES: readOption -> "Hello" == "Hello"
-}
readOption :: IO Option
readOption = do
    catch (do
        line <- getLine
        evaluate (read line))
        ((\_ -> do
            putStrLn "Invalid input"
            readOption) :: SomeException -> IO Option)

{- firstBase
    I/O action that prints availble options in anticipation for a human input (string)
    and jumps to secondBase as output.
    RETURNS: Selected products
    SIDE EFFECTS: Can not terminate untill all options have been selected
    EXAMPLES: firstBase -> "3070" == secondBase "3070"
-}
firstBase :: IO ()
firstBase = do
    putStrLn "\nRTX-Graphicscard\nChoose series:\n  3060\n  3070\n  3080\n"
    choice <- readOption
    if choice == "3060" || choice == "3070" || choice == "3080" then do
        secondBase choice
    else do
        putStrLn "Misspelled"
        firstBase

{- secondBase
    I/O action that prints availble options in anticipation for a human input (string)
    and jumps to crawler as output. 
    RETURNS: Selected products
    SIDE EFFECTS: Can not terminate untill all options have been selected
    EXAMPLES: secondBase "3070" -> "MSI" == crawler "3070" "MSI"
-}
secondBase :: Option -> IO ()
secondBase series = do
    putStrLn "\nChoose Brand:\n  ASUS\n  MSI\n  ZOTAC\n  Gainward\n  Gigabyte\n"
    choice <- readOption
    if choice == "ASUS" || choice == "MSI" || choice == "ZOTAC" || choice == "Gainward"|| choice == "Gigabyte" then do
        crawler choice series
        putStrLn ("Created file for " ++ mergeText choice series ++ "\n")
    else do
        putStrLn "Misspelled"
        secondBase series

{- mergeText string string
    Takes two strings and put them together as one
    RETURNS: String
    EXAMPLES: mergeText "foo" "bar" == "foo bar"
-}
mergeText :: Option -> Option -> Option
mergeText string1 string2 = string1 ++ " " ++ string2