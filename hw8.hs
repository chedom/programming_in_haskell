data Nat = Zero | Succ Nat deriving Show

add :: Nat -> Nat -> Nat
add Zero y = y
add (Succ x) y = add x (Succ y)

mult :: Nat -> Nat -> Nat
mult Zero _ = Zero
mult _ Zero = Zero
mult (Succ Zero) y = y
mult (Succ x) y = add y (mult x y)

