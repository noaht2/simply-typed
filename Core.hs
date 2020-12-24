module Core where
import Control.Monad.ST
import Types
import Parsing

free :: Var -> Expr -> Bool
free x (Left v) = x == v
free x (Right (Left (Abstr y m))) = (x /= y) && free x m
free x (Right (Right (App m n))) = free x m || free x n

beta_reduce :: Expr -> Var -> Expr -> Expr
-- M[N /x] = beta M x N
beta_reduce (Left v) x n
  | v == x = n
  | otherwise = Left v
beta_reduce (Right (Right (App m p))) x n = Right $ Right $ App (beta_reduce m x n) (beta_reduce p x n)
beta_reduce unchanged@(Right (Left (Abstr y m))) x n
  | y == x = unchanged
  | (y /= x) && not (free y n) = Right $ Left $ Abstr y (beta_reduce m x n)
beta_reduce m _ _ = m

beta :: Expr -> Expr
beta v@(Left _) = v
beta (Right (Left (Abstr v m))) = Right $ Left $ Abstr v (beta m)
beta (Right (Right (App (Right (Left (Abstr v m))) n))) = beta_reduce m v n
beta (Right (Right (App m n))) = Right $ Right $ App (beta m) (beta n)

eta :: Expr -> Expr
eta v@(Left _) = v
eta (Right (Right (App m n))) = Right $ Right $ App (eta m) (eta n)
eta (Right (Left (Abstr x (Right (Right (App m y))))))
  | (Left x) == y = m
eta (Right (Left (Abstr v m))) = Right $ Left $ Abstr v (eta m)

simplify_method :: (Expr -> Expr) -> Expr -> Expr
simplify_method pass e = if e == x then e else simplify_method pass x
  where x = pass e

specials = [(parse expr "(λx.(λy.x))", Var "T"),
            (parse expr "(λx.(λy.y))", Var "F"),
            (parse expr "(λc.(λx.(λy.((c x) y))))", Var "if_then_else"),
            (parse expr "(λx.((x (λx.(λy.y))) (λx.(λy.y))))", Var "¬"),
            (parse expr "(λx.(λy.((x y) (λx.(λy.y)))))", Var "and"),
            (parse expr "(λx.(λy.((x (λx.(λy.x))) y)))", Var "or"),
            (parse expr "((λx.(λy.((y ((x x) y))))) (λx.(λy.((y ((x x) y))))))", Var "Θ"),
            (parse expr "(λn.(λs.(λz.(s ((n s) z)))))", Var "succ"),
            (parse expr "(λm.(λn.((m (λn.(λs.(λz.(s ((n s) z)))))) n)))", Var "+"),
            (parse expr "(λm.(λn.((m (+ n)) 0)))", Var "×")]

remove_specials :: [(Expr, Var)] -> Expr -> Expr
remove_specials (special:specials) e = remove_specials specials (beta_reduce e (snd special) (fst special))
remove_specials [] e = e

make_special :: [(Expr, Var)] -> Expr -> Expr
make_special (special:specials) e = if e == fst special then
                                  Left $ snd special else
                                  make_special specials e
make_special [] e = e

make_specials :: Expr -> Expr
make_specials v@(Left _) = v
make_specials f@(Right (Left (Abstr v e))) = if f /= s then s else Right $ Left $ Abstr v (make_specials e)
  where s = make_special specials f
make_specials (Right (Right (App f e))) = Right $ Right $ App (make_specials f) (make_specials e)

simplify :: Expr -> Expr
simplify = simplify_method eta . make_specials . simplify_method beta . remove_specials specials
