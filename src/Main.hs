module Main where

import Characters

import Data.Char (ord)
import Data.Int (Int64)
import Data.Time.Clock.System (SystemTime (systemNanoseconds, systemSeconds), getSystemTime)
import Data.Version (showVersion)
import Paths_TerminalVelocity (version)
import System.IO (BufferMode (NoBuffering), hFlush, hSetBuffering, hSetEcho, stdin, stdout)

data CharDuration = CharDuration
  { duration_before_typed :: Int64
  , char :: Char
  }

data GameState = GameState
  { game_over :: Bool
  , made_mistake :: Bool
  , remaining_chars :: String
  , last_input :: Char
  , num_chars_typed_successfully :: Int
  , start_timestamp :: Int64
  , last_timestamp :: Int64
  , char_durations :: [CharDuration]
  }
  deriving (Show)

defaultGameState :: GameState
defaultGameState =
  GameState
    { game_over = False
    , made_mistake = False
    , remaining_chars = ""
    , last_input = '\0'
    , num_chars_typed_successfully = 0
    , start_timestamp = 0
    , last_timestamp = 0
    , char_durations = []
    }

finalizeGameState :: GameState -> GameState
finalizeGameState gs = gs{char_durations = processed_char_durations}
 where
  processed_char_durations = tail $ reverse cd
  cd =
    if made_mistake gs
      then tail (char_durations gs)
      else char_durations gs

instance Show CharDuration where
  show cd = "Char: " ++ [char cd] ++ ", Duration: " ++ show (duration_before_typed cd)

prettyPrintDurations :: [CharDuration] -> String
prettyPrintDurations = unlines . map show

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
roundLoop gs@GameState{remaining_chars = ""} = return gs{game_over = False, made_mistake = False}
roundLoop gs@GameState{remaining_chars = (current : remaining)} = do
  input <- getChar
  system_nanoseconds <- currentNanoseconds
  let init_timestamp = if start_timestamp gs == 0 then system_nanoseconds else start_timestamp gs
  let elapsed_nanoseconds = system_nanoseconds - init_timestamp
  let new_char_durations = CharDuration{duration_before_typed = system_nanoseconds - last_timestamp gs, char = input} : char_durations gs
  let gs_next = gs{char_durations = new_char_durations, last_input = input, start_timestamp = init_timestamp, last_timestamp = system_nanoseconds}
  if elapsed_nanoseconds >= (60 * 1000000000)
    then return (finalizeGameState gs_next{game_over = True, made_mistake = False})
    else
      if (ord input == 27) || (input /= current)
        then return (finalizeGameState gs_next{game_over = True, made_mistake = True})
        else do
          putChar input
          hFlush stdout
          roundLoop gs_next{remaining_chars = remaining, num_chars_typed_successfully = num_chars_typed_successfully gs + 1}

roundSucceded :: IO ()
roundSucceded = do
  putStrLn ""
  putStrLn ""

roundFailed :: GameState -> IO ()
roundFailed GameState{remaining_chars = ""} = error "Something went wrong."
roundFailed gs@GameState{remaining_chars = (failed_on : _)} = do
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
  gs_next <- roundLoop gs{remaining_chars = str}

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
          putStrLn "   Time's up!"
      return gs_next

startGame :: IO ()
startGame = do
  putStrLn ("   Terminal Velocity v" ++ showVersion version)
  putStrLn ""
  putStrLn " * Once you start typing, a one-minute countdown begins."
  putStrLn " * Try to type as many characters as possible before the time runs out."
  putStrLn " * If you make a mistake, the game ends immediately."
  putStrLn ""

debugPrint :: GameState -> IO ()
debugPrint gs = do
  putStrLn (prettyPrintDurations (char_durations gs))
  putStrLn ""

endGame :: GameState -> IO ()
endGame gs = do
  putStrLn ""
  putStrLn ("   You successfully typed " ++ show (num_chars_typed_successfully gs) ++ " characters.")
  putStrLn ""
  debugPrint gs

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  startGame
  result <- createRound defaultGameState
  endGame result
