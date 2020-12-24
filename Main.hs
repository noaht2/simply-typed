module Main where
import Core
import Parsing
import Types

two_plus_three = "((+ (λs.(λz.(s (s z))))) (λs.(λz.(s (s (s z))))))"

main :: IO ()
main = do {input <- getLine;
           putStrLn $ display $ simplify $ parse expr input}
