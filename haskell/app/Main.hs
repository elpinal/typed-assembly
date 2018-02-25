module Main where

import Tal0
import Tal0.Parser

import Control.Monad

main :: IO ()
main = forever $ do
  putStr "kind: "
  line <- getLine
  if line == "heap"
    then readHeap
    else getFile >>= print

readHeap :: IO ()
readHeap = do
  line <- getLine
  case parseHeap line of
    Right h -> print h
    Left e -> print e

getFile :: IO (Either ParseError File)
getFile = parseFile <$> getLine
