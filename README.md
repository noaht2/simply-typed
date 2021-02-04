An implementation of the simply typed lambda calculus in Haskell extended with product types and sum types/ordered pairs and tagged unions.

To build:
```sh
ghc Main
```

Example of use:
```
$ ./Main
(λ(b:((B→B)+B)).(case (b:((B→B)+B)) of (λ(t:(B→B)).(Right (B→B) (f:B))) or (λ(f:B).(Left (t:(B→B)) B))))
(λ(b:((B→B)+B)).(case (b:((B→B)+B)) of (λ(t:(B→B)).(Right (B→B) (f:B))) or (λ(f:B).(Left (t:(B→B)) B)))) 	: 	(((B→B)+B)→((B→B)+B)) 	=ᵦₑ 	(λ(b:((B→B)+B)).(case (b:((B→B)+B)) of (λ(t:(B→B)).(Right (B→B) (f:B))) or (λ(f:B).(Left (t:(B→B)) B))))
```
