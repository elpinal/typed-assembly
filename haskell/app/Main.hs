module Main where

import Tal0.Parser

import Control.Monad

main :: IO ()
main = forever $ do
  line <- getLine
  case parseHeap line of
    Right h -> print h
    Left e -> print e

readFile :: IO File
readFile = do
  line <- getLine
  parseFile line
