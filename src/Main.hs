module Main where

import System.IO

import Types
import Evaluator
import Parser

import Examples
import GCD

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering

