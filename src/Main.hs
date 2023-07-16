module Main where

import Characters

import Data.Char
import System.IO

roundStart :: String -> IO ()
roundStart str = do
  putStrLn ("Type this: " ++ str)
  putStr "---------> "
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
  putStrLn ("Failed on character '" ++ [failed_on] ++ "'")

  if input `elem` charCombined
    then putStrLn ("          You typed '" ++ [input] ++ "'")
    else putStr ""

createRound :: IO ()
createRound = do
  str <- randomString 10 charCombined
  roundStart str
  (result, remaining, input) <- roundLoop str

  if result
    then do
      roundSucceded
      createRound
    else roundFailed remaining input

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  createRound
