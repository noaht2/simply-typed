module Main where
import Core
import Parsing
import Display

two_plus_three = "(((λm.(λn.((m (λn.(λs.(λz.(s ((n s) z)))))) n))) (λs.(λz.(s (s z))))) (λs.(λz.(s (s (s z))))))"

main :: IO ()
main = do {input <- getLine;
           print (simplify (parse expr input))}
