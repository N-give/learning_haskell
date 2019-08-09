module Main where

import Lib
import System.Environment

main :: IO ()
main = getArgs >>= print . haqify . head
