module Main where

import Characters

import Data.Char (ord)
import Data.Int (Int64)
import Data.Time.Clock.System (SystemTime (systemNanoseconds, systemSeconds), getSystemTime)
import Data.Version (showVersion)
import Paths_TerminalVelocity (version)
import System.IO (hFlush, hSetBuffering, stdout, stdin, hSetEcho, BufferMode(NoBuffering))

currentNanoseconds :: IO Int64
currentNanoseconds = do
  systemTime <- getSystemTime
  let seconds = systemSeconds systemTime
      nanoseconds = systemNanoseconds systemTime
  return ((seconds * 1000000000 + fromIntegral nanoseconds) :: Int64)

roundStart :: String -> IO ()
roundStart str = do
  putStrLn "   Type this: "
  putStrLn ("              " ++ str)
  putStr "            > "
  hFlush stdout

roundLoop :: String -> Int -> Int64 -> IO (Bool, Bool, String, Char, Int, Int64)
roundLoop "" chars_typed total_nanoseconds = return (True, False, "", '.', chars_typed, total_nanoseconds)
roundLoop (current : remaining) chars_typed total_nanoseconds = do
  input <- getChar
  system_nanoseconds <- currentNanoseconds
  let start_timestamp = if total_nanoseconds == 0 then system_nanoseconds else total_nanoseconds
  let elapsed_nanoseconds = system_nanoseconds - start_timestamp
  if elapsed_nanoseconds >= (60 * 1000000000)
    then return (False, False, current : remaining, input, chars_typed, start_timestamp)
    else
      if (ord input == 27) || (input /= current)
        then return (False, True, current : remaining, input, chars_typed, start_timestamp)
        else do
          putChar input
          hFlush stdout
          roundLoop remaining (chars_typed + 1) start_timestamp

roundSucceded :: IO ()
roundSucceded = do
  putStrLn ""
  putStrLn ""

roundFailed :: String -> Char -> IO ()
roundFailed "" _ = error "Something went wrong."
roundFailed (failed_on : _) input = do
  putStrLn ""
  putStrLn ""
  putStrLn ("   You should have typed '" ++ [failed_on] ++ "'")

  if input `elem` charCombined
    then putStrLn ("       ... but you typed '" ++ [input] ++ "'")
    else putStr ""

createRound :: Int -> Int64 -> IO Int
createRound chars_typed start_timestamp = do
  str <- randomSentence 40 9
  roundStart str
  (keep_going, failed, remaining, input, total_chars_typed, total_nanoseconds) <- roundLoop str chars_typed start_timestamp

  if keep_going
    then do
      roundSucceded
      createRound total_chars_typed total_nanoseconds
    else do
      if failed then
        roundFailed remaining input
      else do 
        putStrLn ""
        putStrLn ""
        putStrLn " Time's up!"
      return total_chars_typed

startGame :: IO ()
startGame = do
  putStrLn ("   Terminal Velocity v" ++ showVersion version)
  putStrLn ""
  putStrLn " * A one minute countdown begins once you start typing."
  putStrLn " * Type as many characters as you can before the time is up."
  putStrLn " * If you make a mistake the game ends."
  putStrLn ""

endGame :: Int -> IO ()
endGame total = do
  putStrLn ""
  putStrLn ("   You typed out a total of " ++ show total ++ " characters correctly.")
  putStrLn ""

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  startGame
  result <- createRound 0 0
  endGame result
