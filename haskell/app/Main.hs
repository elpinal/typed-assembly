module Main where

import Tal0.Parser

main :: IO ()
main = do
  line <- getLine
  case parseHeap line of
    Right h -> print h
    Left e -> print e
