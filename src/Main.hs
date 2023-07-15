module Main where

import System.Random (randomRIO)
import Control.Monad (replicateM)
import System.IO
import Data.Char

charLetters :: [Char]
charLetters = ['a'..'z']

charNumbers :: [Char]
charNumbers = ['0'..'9']

charSpecial :: [Char]
charSpecial = ['@', '+', '"']

charCombined :: [Char]
charCombined = charLetters ++ charNumbers ++ charSpecial

randomChar :: [Char] -> IO Char
randomChar char_list = (char_list !!) <$> randomRIO (1, length char_list - 1)

randomString :: Int -> [Char] -> IO [Char]
randomString count char_list = replicateM count (randomChar char_list)

mainLoop :: IO ()
mainLoop = do
  c <- getChar
  if ord c == 27 then 
    return ()
  else do
    putChar c
    hFlush stdout
    mainLoop

startGame :: String -> IO ()
startGame str = do
  putStrLn ("Type this: " ++ str)
  putStr   "         : " 
  hFlush stdout

endGame :: IO ()
endGame = do
  putStrLn ""
  putStrLn "Game Over."

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  str <- randomString 30 charCombined
  startGame str
  mainLoop
  endGame
  hFlush stdout
