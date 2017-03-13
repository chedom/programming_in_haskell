--1
mapFilter :: (a -> b) -> (a -> Bool) -> [a] -> [b]
mapFilter f p = map f . filter p

--2
--a all
all1 :: (a -> Bool) -> [a] -> Bool
all1 _ [] = True
all1 p (x:xs)   | p x = all1 p  xs
                | otherwise = False

--b any
any1 :: (a -> Bool) -> [a] -> Bool
any1 _ [] = False
any1 p (x:xs)   | p x = True
                | otherwise = any1 p xs

--c takeWhile
takeWhile1 :: (a -> Bool) -> [a] -> [a]
takeWhile1 _ [] = []
takeWhile1 p (x:xs) | p x = x : takeWhile1 p xs
                    | otherwise = [] 

--d dropWhile
dropWhile1 :: (a -> Bool) -> [a] -> [a]
dropWhile1 _ [] = []
dropWhile1 p (x:xs) | p x = dropWhile p xs
                    | otherwise = x : xs

--3 redefine map and filter using foldr
map1 :: (a -> b) -> [a] -> [b]
map1 f = foldr (\x acc -> f x : acc) []

filter1 :: (a -> Bool) -> [a] -> [a]
filter1 p = foldr (\x acc -> case p x of
                        True -> x : acc
                        False -> acc             
                  ) []

--filter1 p = foldr (\x xs -> if p x then x:xs else xs) []

--4
dec2int :: [Int] -> Int
dec2int xs = foldl (\acc (elem, weight) -> acc + elem * weight) 0 (zip (reverse xs) weights)
    where weights = iterate (*10) 1

--dec2int = foldl (\x y -> 10*x + y) 0

--5 define curry and uncarry functions
curry1 :: ((a, b) -> c) -> a -> b -> c
curry1 f = \x -> \y -> f (x,y)

uncurry1 :: (a -> b -> c) -> ((a, b) -> c)
uncurry1 f =  \(x, y) -> f x y   

--6  unfold func
unfold p h t x  | p x = []
                | otherwise = h x : unfold p h t (t x)

chop8 :: [Int] -> [[Int]]
chop8 = unfold null (take 8) (drop 8)

map2 :: (a -> b) -> [a] -> [b]
map2 f = unfold null (f . head) tail

iterate2 f = unfold (\ _ -> False) id f

--9 altMap func
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f1 f2 [] = []
altMap f1 f2 (x:xs) = f1 x : altMap f2 f1 xs


