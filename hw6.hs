--1 factorial
fac :: Int -> Int
fac 0 = 1
fac n | n>0 = n*fac(n-1)


--2 sum of the non-negative integers
sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n-1)

--3 exponential operator for non-negative integers
exp' :: Int -> Int -> Int
n `exp'` 1 = n
n `exp'` k = n*(n `exp'` (k-1))


--4 Euclid's algorithm
euclid :: Int -> Int -> Int
euclid n k  | n==k = n
            | otherwise = if n > k then euclid (n-k) k else euclid n (k-n)

--6

--a and function
and':: [Bool] -> Bool
and' [] = True
and' (x:xs) | x = and' xs
            | otherwise = False

--b concat function
concat' :: [[a]] -> [a]
concat' [] = []
concat' (xs: xss) = xs ++ concat' xss

--c replicate function
replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = x : replicate (n-1) x

--d select nth element of a list
nth' :: [a] -> Int -> a
nth' (x:_) 0 = x
nth' (_:xs) n = nth' xs (n-1)

--e decide if a value is an element of a list
elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' x (y:yx)  | x == y = True
                | otherwise = elem' x yx

--7 merge two sorted lists to give sorted list
merge' xs [] = xs
merge' [] ys = ys
merge' (x:xs) (y:ys)    | x <= y = x : merge' xs (y:ys)
                        | otherwise = y: merge' (x:xs) ys


--8 define merge sort
halve :: [a] -> ([a], [a])
halve xs = (take h xs, drop h xs)
            where 
                h = length xs `div` 2

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge' (msort l) (msort r)
            where
                (l, r) = halve xs

--9
-- a sum of numbers
sum' :: [Int] -> Int
sum' [] = 0
sum' (x:xs) = x + sum' xs

--b take function
take' :: Int -> [a] -> [a]
take' 0 xs = []
take' _ [] = []
take' n (x:xs) = x: take' (n-1) xs

--c last function
last' :: [a] -> a
last' (x:xs)    | null xs = x
                | otherwise = last xs            

