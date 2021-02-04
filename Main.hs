module Main where
import Core
import Parsing
import Types

not_func = "(λ(b:((B→B)+B)).(case (b:((B→B)+B)) of (λ(t:(B→B)).(Right (B→B) (f:B))) or (λ(f:B).(Left (t:(B→B)) B))))"

not_false = "((λ(b:((B→B)+B)).(case (b:((B→B)+B)) of (λ(t:(B→B)).(f:((B→B)+B))) or (λ(f:B).(t:((B→B)+B))))) (f:((B→B)+B)))"

simple = "(x:B)"

false = "(f:((B→B)+B))"

present :: String -> IO ()
present s = if well_typed m then putStr (display m) >> putStr " \t: \t" >> putStr (display_type $ get_type m) >> putStr " \t=ᵦₑ \t" >> putStrLn (display $ simplify m) else error "Not well typed!"
  where m = parse expr s

main :: IO ()
main = do {input <- getLine;
           present input}
