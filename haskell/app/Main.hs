module Main where

import Tal0
import Tal0.Parser

import Control.Monad

main :: IO ()
main = forever $ do
  line <- getLine
  if line == "heap"
    then readHeap
    else readFile >>= print

readHeap :: IO ()
readHeap = do
  line <- getLine
  case parseHeap line of
    Right h -> print h
    Left e -> print e

readFile :: IO (Either ParseError File)
readFile = parseFile <$> getLine
