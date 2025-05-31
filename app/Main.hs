module Main where

import qualified MyLib (someFunc)
import CLParser


main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  MyLib.someFunc
