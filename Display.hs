module Display where
import Types
import Parsing
import Core

instance Show Expr where
  show (Var v) = v
  show (Abstr v@(Var _) e) = "(λ" ++ show v ++ "." ++ show e ++ ")"
  show (App f e) = "(" ++ show f ++ " " ++ show e ++ ")"
  show (Special _ v@(Var _)) = show v
