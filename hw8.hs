data Nat = Zero | Succ Nat deriving Show

--1

add :: Nat -> Nat -> Nat
add Zero y = y
add (Succ x) y = add x (Succ y)

mult :: Nat -> Nat -> Nat
mult Zero _ = Zero
mult _ Zero = Zero
mult (Succ Zero) y = y
mult (Succ x) y = add y (mult x y)

--2 see chapter8.hs occurs'

--3

data Tree a = Leaf a | Node (Tree a) (Tree a)

balanced :: Tree a -> Bool
balanced (Leaf _) = True
balanced (Node l r) = number_of_leaves l == number_of_leaves r

number_of_leaves :: Tree a -> Int
number_of_leaves (Leaf _) = 1
number_of_leaves (Node l r) = number_of_leaves l + number_of_leaves r


