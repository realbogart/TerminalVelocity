module Main where

import Characters

import Data.Char (ord)
import Data.Int (Int64)
import Data.Time.Clock.System (SystemTime (systemNanoseconds, systemSeconds), getSystemTime)
import Data.Version (showVersion)
import Paths_TerminalVelocity (version)
import System.IO (BufferMode (NoBuffering), hFlush, hSetBuffering, hSetEcho, stdin, stdout)

data GameState = GameState {
  game_over :: Bool,
  made_mistake :: Bool,
  remaining_chars :: String,
  last_input :: Char,
  num_chars_typed :: Int,
  start_timestamp :: Int64
}

defaultGameState :: GameState
defaultGameState = GameState {
  game_over = False,
  made_mistake = False,
  remaining_chars = "",
  last_input = '\0',
  num_chars_typed = 0,
  start_timestamp = 0
}

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

roundLoop :: GameState -> IO GameState
roundLoop gs@GameState { remaining_chars = "" } = return gs { game_over = False, made_mistake = False } 
roundLoop gs@GameState { remaining_chars = (current : remaining) } = do
  input <- getChar
  system_nanoseconds <- currentNanoseconds
  let init_timestamp = if start_timestamp gs == 0 then system_nanoseconds else start_timestamp gs
  let elapsed_nanoseconds = system_nanoseconds - init_timestamp
  if elapsed_nanoseconds >= (60 * 1000000000)
    then return gs { game_over = True, made_mistake = False }
    else
      if (ord input == 27) || (input /= current)
        then return gs { game_over = True, made_mistake = True, last_input = input }
        else do
          putChar input
          hFlush stdout
          roundLoop gs { remaining_chars = remaining, num_chars_typed = num_chars_typed gs + 1, start_timestamp = init_timestamp }

roundSucceded :: IO ()
roundSucceded = do
  putStrLn ""
  putStrLn ""

roundFailed :: GameState -> IO ()
roundFailed GameState { remaining_chars = "" } = error "Something went wrong."
roundFailed gs@GameState { remaining_chars = (failed_on : _)} = do
  putStrLn ""
  putStrLn ""
  putStrLn ("   You should have typed '" ++ [failed_on] ++ "'")

  let input = last_input gs

  if input `elem` charCombined
    then putStrLn ("       ... but you typed '" ++ [input] ++ "'")
    else putStr ""

createRound :: GameState -> IO GameState
createRound gs = do
  str <- randomSentence 40 9
  roundStart str
  gs_next <- roundLoop gs { remaining_chars = str }

  if not (game_over gs_next)
    then do
      roundSucceded
      createRound gs_next
    else do
      if made_mistake gs_next
        then roundFailed gs_next
        else do
          putStrLn ""
          putStrLn ""
          putStrLn " Time's up!"
      return gs_next

startGame :: IO ()
startGame = do
  putStrLn ("   Terminal Velocity v" ++ showVersion version)
  putStrLn ""
  putStrLn " * A one minute countdown begins once you start typing."
  putStrLn " * Type as many characters as you can before the time is up."
  putStrLn " * If you make a mistake the game ends."
  putStrLn ""

endGame :: GameState -> IO ()
endGame gs = do
  putStrLn ""
  putStrLn ("   You typed out a total of " ++ show (num_chars_typed gs) ++ " characters correctly.")
  putStrLn ""

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  startGame
  result <- createRound defaultGameState
  endGame result
