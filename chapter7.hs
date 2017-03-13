add :: Int -> Int -> Int
add = \x -> \y -> x+y

twice :: (a -> a) -> a -> a
twice f x = f (f x)

map1 :: (a -> b) -> [a] -> [b]
map1 f xs = [f x | x <- xs]

map2 :: (a -> b) -> [a] -> [b]
map2 f [] = []
map2 f (x:xs) = f x : map2 f xs

filter1 :: (a -> Bool) -> [a] -> [a]
filter1 p xs = [x | x <- xs, p x]

filter2 :: (a -> Bool) -> [a] -> [a]
filter2 p [] = []
filter2 p (x:xs) | p x = x : filter2 p xs
                | otherwise = filter2 p xs

sumsqreven :: [Int] -> Int
sumsqreven ns = sum (map (^2) (filter even ns))

all1 :: (a -> Bool) -> [a] -> Bool
all1 p []       = True
all1 p (x:xs) | p x = all1 p xs
              | otherwise = False  

any1 :: (a -> Bool) -> [a] -> Bool
any1 p [] = False
any1 p (x:xs) | p x = True
              | otherwise = any1 p xs  

takeWhile1 :: (a -> Bool) -> [a] -> [a]
takeWhile1 p [] = []
takeWhile1 p (x:xs) | p x = x : takeWhile1 p xs
                    | otherwise = []

dropWhile1 :: (a -> Bool) -> [a] -> [a]
dropWhile1 p [] = []
dropWhile1 p (x:xs) | p x = dropWhile1 p xs
                    | otherwise = x:xs

sum1 :: Num a => [a] -> a
sum1 = foldr (+) 0

product1 :: Num a => [a] -> a
product1 = foldr (*) 1

or1 :: [Bool] -> Bool
or1 = foldr (||) False

and1 :: [Bool] -> Bool
and1 = foldr (&&) True

foldr11 :: (a -> b -> b) -> b -> [a] -> b
foldr11 f v [] = v
foldr11 f v (x:xs) = f x (foldr11 f v xs)

foldl11 :: (a -> b -> a) -> a -> [b] -> a
foldl11 f v [] = v
foldl11 f v (x:xs) = foldl11 f (f v x) xs
