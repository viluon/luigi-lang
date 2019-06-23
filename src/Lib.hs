module Lib
    ( main
    ) where

import Parser
import Parser.Wrapper

main :: IO ()
main = do
    line <- getLine
    putStrLn (show (runParser "stdin" line parseBlock))
