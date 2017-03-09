--1
sumOfSquares :: Int -> Int
sumOfSquares n = sum [x ^ 2 | x <- [1..n]]

--2
grid :: Int -> Int -> [(Int, Int)]
grid n m = [(x, y) | x <- [0..n], y <- [0..m]]

--3
square :: Int -> [(Int, Int)]
square n = [(x, y) | (x, y) <- grid n n, x /= y]

--4
replicate' :: Int -> a -> [a]
replicate' n s = [s | _ <- [1..n]]

--5
pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) | x <- [1..n], 
    y <- [1..n], 
    z <- [1..n], 
    x^2 + y^2 == z ^ 2 && all (\a -> a <= 10) [x, y, z]]

--6
factors :: Int -> [Int]
factors n = [x | x <- [1.. (n - 1)], n `mod` x == 0]

perfect n = sum (factors n) == n

perfects n = [x | x <- [1..n], perfect x]

--7
nestGen = concat [[(x, y) | x <- [1, 2]] | y <- [3, 4]]

--8
find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (k',v) <- t, k == k']

positions x xs = find x (zip xs [0..])

--9
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x * y | (x, y) <- zip xs ys]