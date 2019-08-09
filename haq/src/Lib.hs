module Lib
    ( someFunc
    , haqify
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

haqify s = "Haq! " ++ s
