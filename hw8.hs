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

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

balanced :: Tree a -> Bool
balanced (Leaf _) = True
balanced (Node l r) = number_of_leaves l == number_of_leaves r

number_of_leaves :: Tree a -> Int
number_of_leaves (Leaf _) = 1
number_of_leaves (Node l r) = number_of_leaves l + number_of_leaves r

--4
balance :: [a] -> Tree a
balance [x] = Leaf x
balance xs  = Node (balance l) (balance r)
    where 
        (l, r) = halves xs 

halves :: [a] -> ([a], [a])
halves [] = ([], [])
halves xs = (take halve xs, drop halve xs)
    where halve = length xs `div` 2

--5
data Expr = Val Int | Add Expr Expr

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f g (Add e1 e2) = g (folde f g e1) (folde f g e2)
folde f g (Val e) = f e  

--6
eval :: Expr -> Int
eval = folde id (+)

size :: Expr -> Int
size (Val _) = 1
size (Add e1 e2) = size e1 + size e2


