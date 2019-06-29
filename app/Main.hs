module Main where

import           Lib
import           System.IO
import           Control.Monad                  ( forever )

prompt :: String -> IO String
prompt text = do
  putStr text
  hFlush stdout
  getLine

main :: IO ()
main = putStrLn "Brainfuck interpreter" *> forever (prompt ">>> " >>= run)
