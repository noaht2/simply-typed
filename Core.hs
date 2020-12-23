module Core where
import Control.Monad.ST
import Data.STRef
import Types
import Parsing

instance Eq Expr where
  (Var v) == (Var u) = v == u
  f@(Abstr x@(Var _) m) == g@(Abstr y@(Var _) n)
    | x == y = m == n
--     | x /= y && m == n = False
    | otherwise = False -- alpha g x == f
  (App m n) == (App o p) = m == o && n == p
  (Special s x) == (Special s' x') = s == s' && x == x'
  _ == _ = False

free :: Var -> Expr -> Bool
free x@(Var _) v@(Var _) = x == v
free x@(Var _) (Abstr y m) = (x /= y) && free x m
free x@(Var _) (App m n) = free x m || free x n

-- bound :: Var -> Expr -> Bool
-- bound x@(Var _) (Abstr v@(Var _) _) = x == v
-- bound x@(Var _) (App m n) = bound x m || bound x n
-- bound (Var _) (Var _) = False

-- replace :: Expr -> Var -> Var -> Expr
-- replace v@(Var _) x@(Var _) y@(Var _)
--   | v == x = y
--   | otherwise = v
-- replace (Abstr v@(Var _) m) x@(Var _) y@(Var _)
--   | v == x = (Abstr y (replace m x y))
--   | otherwise = (Abstr v (replace m x y))
-- replace (App m n) x@(Var _) y@(Var _) = (App (replace m x y) (replace n x y))

-- alpha :: Abstr -> Var -> Abstr
-- alpha unchanged@(Abstr x@(Var _) m) y@(Var _)
--   | x == y = unchanged
--   | not (bound x m) && not (bound y m) && not (free y m) = replace unchanged x y
--   | otherwise = unchanged

beta_reduce :: Expr -> Var -> Expr -> Expr
-- M[N /x] = beta M x N
beta_reduce v@(Var _) x@(Var _) n
  | v == x = n
  | otherwise = v
beta_reduce (App m p) x@(Var _) n = (App (beta_reduce m x n) (beta_reduce p x n))
beta_reduce unchanged@(Abstr y m) x@(Var _) n
  | y == x = unchanged
  | (y /= x) && (not (free y n)) = (Abstr y (beta_reduce m x n))
beta_reduce m _ _ = m

beta :: Expr -> Expr
beta v@(Var _) = v
beta (Abstr v@(Var _) m) = Abstr v (beta m)
beta (App (Abstr v@(Var _) m) n) = beta_reduce m v n
beta (App m n) = App (beta m) (beta n)

eta :: Expr -> Expr
eta v@(Var _) = v
eta (App m n) = (App (eta m) (eta n))
eta (Abstr x@(Var _) (App m y))
  | x == y = m
eta (Abstr v@(Var _) m) = (Abstr v (eta m))
eta s@(Special _ _) = s

simplify_method :: (Expr -> Expr) -> Expr -> Expr
simplify_method pass e = if e == x then e else simplify_method pass x
  where x = pass e

specials = [(parse expr "(λx.(λy.x))", (Var "T")),
            (parse expr "(λx.(λy.y))", (Var "F")),
            (parse expr "(λc.(λx.(λy.((c x) y))))", (Var "if_then_else")),
            (parse expr "(λx.((x (λx.(λy.y))) (λx.(λy.y))))", (Var "¬")),
            (parse expr "(λx.(λy.((x y) (λx.(λy.y)))))", (Var "and")),
            (parse expr "(λx.(λy.((x (λx.(λy.x))) y)))", (Var "or")),
            (parse expr "((λx.(λy.((y ((x x) y))))) (λx.(λy.((y ((x x) y))))))", (Var "Θ")),
            (parse expr "(λn.(λs.(λz.(s ((n s) z)))))", (Var "succ")),
            (parse expr "(λm.(λn.((m (λn.(λs.(λz.(s ((n s) z)))))) n)))", (Var "+")),
            (parse expr "(λm.(λn.((m (+ n)) 0)))", (Var "×"))]

remove_specials :: [(Expr, Expr)] -> Expr -> Expr
remove_specials (special:specials) e = remove_specials specials (beta_reduce e (snd special) (fst special))
remove_specials [] e = e

make_special :: [(Abstr, Var)] -> Expr -> Expr
make_special (special:specials) e = if e == fst special then
                                  snd special else
                                  make_special specials e
make_special [] e = e

make_specials :: Expr -> Expr
make_specials v@(Var _) = v
make_specials f@(Abstr v@(Var _) e) = if f /= s then s else Abstr v (make_specials e)
  where s = make_special specials f
make_specials (App f e) = App (make_specials f) (make_specials e)

simplify :: Expr -> Expr
simplify = (simplify_method eta) . (make_specials) . (simplify_method beta) . (remove_specials specials)
