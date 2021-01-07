module Core where
import Types
import Parsing

free :: String -> Expr -> Bool
free x (Var v t) = x == v
free x (Abstr y _ m) = (x /= y) && free x m
free x (App m n) = free x m || free x n
free x Fst = True
free x Snd = True
free x (Pair m n) = free x m || free x n
free x (Union _ _ m) = free x m
free x (Case u l r) = free x u || free x l || free x r

beta_reduce :: String -> Expr -> Expr -> Expr
-- M[N /x] = beta x N M
beta_reduce x n (Var v t)
  | v == x = n
  | otherwise = Var v t
beta_reduce x n unchanged@(Abstr y t m)
  | y == x = unchanged
  | (y /= x) && not (free y n) = Abstr y t (beta_reduce x n m)
beta_reduce x n m = apply (beta_reduce x n) m

evaluate :: Expr -> Expr
evaluate (App (Abstr v t m) n) = beta_reduce v n m
evaluate (Case (Union t b m) l r) = evaluate $ App f m
  where f
          | b = l
          | otherwise = r
evaluate e = apply evaluate e

eta :: Expr -> Expr
eta (Abstr x t (App m y))
  | (Var x t) == y = m
eta m = apply eta m

simplify_method :: (Expr -> Expr) -> Expr -> Expr
simplify_method pass e = if e == x then e else simplify_method pass x
  where x = pass e

specials = [(parse expr "(Left (t:(B→B)) B)", parse expr "(t:((B→B)+B))"),
            (parse expr "(Right (B→B) (f:B))", parse expr "(f:((B→B)+B))")]
           
remove_specials :: [(Expr, Expr)] -> Expr -> Expr
remove_specials ((value, (Var x t)):specials) e = remove_specials specials $ beta_reduce x value e
remove_specials [] e = e

make_special :: [(Expr, Expr)] -> Expr -> Expr
make_special ((value, var):specials) e = if e == value then
                                           var else
                                           make_special specials e
make_special [] e = e

make_specials :: Expr -> Expr
make_specials m = if m /= m' then m' else apply make_specials m
  where m' = make_special specials m

simplify :: Expr -> Expr
simplify = simplify_method eta . make_specials . simplify_method evaluate . remove_specials specials
