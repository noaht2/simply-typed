module Types where

data Var = Var {name :: String}
data Abstr = Abstr {param :: Var, body :: Expr}
data App = App {func :: Expr, arg :: Expr}

type Expr = Either Var (Either Abstr App)

instance Eq Var where
  (Var v) == (Var u) = v == u

instance Eq Abstr where
  (Abstr x m) == (Abstr y n)
    | x == y = m == n
    | otherwise = False

instance Eq App where
  (App m n) == (App o p) = m == o && n == p

display :: Expr -> String
display (Left (Var v)) = v
display (Right (Left (Abstr v e))) = "(λ" ++ display (Left v) ++ "." ++ display e ++ ")"
display (Right (Right (App f e))) = "(" ++ display f ++ " " ++ display e ++ ")"
