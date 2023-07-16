module Main where

import Characters

import Data.Char
import Data.Version (showVersion)
import Paths_typing_practice (version)
import System.IO

roundStart :: String -> IO ()
roundStart str = do
  putStrLn " Type this: "
  putStrLn ("            " ++ str)
  putStr "          > "
  hFlush stdout

roundLoop :: String -> Int -> IO (Bool, String, Char, Int)
roundLoop "" chars_typed = return (True, "", '.', chars_typed)
roundLoop (current : remaining) chars_typed = do
  input <- getChar
  if (ord input == 27) || (input /= current)
    then return (False, current : remaining, input, chars_typed)
    else do
      putChar input
      hFlush stdout
      roundLoop remaining (chars_typed + 1)

roundSucceded :: IO ()
roundSucceded = do
  putStrLn ""
  putStrLn ""

roundFailed :: String -> Char -> IO ()
roundFailed "" _ = error "Something went wrong."
roundFailed (failed_on : _) input = do
  putStrLn ""
  putStrLn ""
  putStrLn (" You should have typed '" ++ [failed_on] ++ "'")

  if input `elem` charCombined
    then putStrLn ("     ... but you typed '" ++ [input] ++ "'")
    else putStr ""

createRound :: Int -> IO Int
createRound chars_typed = do
  str <- randomSentence 7 9
  roundStart str
  (result, remaining, input, total_chars_typed) <- roundLoop str chars_typed

  if result
    then do
      roundSucceded
      createRound total_chars_typed
    else do
      roundFailed remaining input
      return total_chars_typed

startGame :: IO ()
startGame = do
  putStrLn "------------------------------------------------------------"
  putStrLn (" Typing Practice v" ++ showVersion version)
  putStrLn "------------------------------------------------------------"
  putStrLn " * A one minute countdown will start when you start typing."
  putStrLn " * Type out as much of the text as you can."
  putStrLn " * If you make a mistake the game ends."
  putStrLn "------------------------------------------------------------"

endGame :: Int -> IO ()
endGame total = do
  putStrLn ""
  putStrLn (" You typed out a total of " ++ show total ++ " characters correctly.")
  putStrLn "------------------------------------------------------------"

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  startGame
  result <- createRound 0
  endGame result
