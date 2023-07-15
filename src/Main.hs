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

startGame :: String -> IO ()
startGame str = do
  putStrLn ("Type this: " ++ str)
  putStr   "         : " 
  hFlush stdout

mainLoop :: String -> IO ()
mainLoop "" = return ()
mainLoop (current:remaining) = do
  input <- getChar
  if (ord input == 27) || (input /= current) then 
    return ()
  else do
    putChar input
    hFlush stdout
    mainLoop remaining

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
  mainLoop str
  endGame
  hFlush stdout
