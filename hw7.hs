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

