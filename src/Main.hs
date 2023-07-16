module Main where

import Characters

import Paths_typing_practice (version)
import Data.Version (showVersion)
import Data.Char
import System.IO

roundStart :: String -> IO ()
roundStart str = do
  putStrLn (" Type this: " ++ str)
  putStr "          > "
  hFlush stdout

roundLoop :: String -> IO (Bool, String, Char)
roundLoop "" = return (True, "", '.')
roundLoop (current : remaining) = do
  input <- getChar
  if (ord input == 27) || (input /= current)
    then return (False, current : remaining, input)
    else do
      putChar input
      hFlush stdout
      roundLoop remaining

roundSucceded :: IO ()
roundSucceded = do
  putStrLn ""
  putStrLn ""

roundFailed :: String -> Char -> IO ()
roundFailed "" _ = error "Something went wrong."
roundFailed (failed_on : _) input = do
  putStrLn ""
  putStrLn ""
  putStrLn (        " You should have typed '" ++ [failed_on] ++ "'")

  if input `elem` charCombined
    then putStrLn ( "      ...but you typed '" ++ [input] ++ "'")
    else putStr ""

createRound :: IO ()
createRound = do
  str <- randomSentence 7 9
  roundStart str
  (result, remaining, input) <- roundLoop str

  if result
    then do
      roundSucceded
      createRound
    else roundFailed remaining input

startGame :: IO ()
startGame = do
  putStrLn "------------------------------------------------------------"  
  putStrLn (" Typing Practice v" ++ showVersion version)
  putStrLn "------------------------------------------------------------"  
  putStrLn " A one minute timer will start as soon as you start typing."  
  putStrLn " Type out as much of the text as you can."  
  putStrLn " If you make a mistake the game ends."  
  putStrLn "------------------------------------------------------------"  

endGame :: IO ()
endGame = do
  putStrLn "------------------------------------------------------------"

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  startGame
  createRound
  endGame
