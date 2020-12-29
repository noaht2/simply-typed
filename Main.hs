module Main where
import Core
import Parsing
import Types

main :: IO ()
main = do {input <- getLine;
           putStrLn $ display $ simplify $ parse expr input}
