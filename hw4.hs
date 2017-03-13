--1
halve :: [a] -> ([a], [a])

halve xs = (take l xs, drop l xs)
    where 
        l = length xs `div` 2

--2
third1 :: [a] -> a

third1 xs = head (tail (tail xs))

third2 :: [a] -> a

third2 xs = xs !! 2

third3 :: [a] -> a

third3 (_ : _ : x : _) = x

--3
safetail1 :: [a] -> [a]
safetail1 xs = if null xs then xs else tail xs

safetail2 :: [a] -> [a]
safetail2 xs    | null xs = xs
                | otherwise = tail xs

safetail3 :: [a] -> [a]  
safetail3 [] = []
safetail3 xs = tail xs

--4
(||) :: Bool -> Bool -> Bool
True || _ = True
False || b = b

--5
conj1 :: Bool -> Bool -> Bool
conj1 x y = if x == False then False 
        else 
            if y == True then True else False

--6
conj2 :: Bool -> Bool -> Bool
conj2 x y = if x then y else False

--7
mult :: Int -> Int -> Int -> Int
mult = \x -> \y -> \z -> x * y * z

--8
luhnDouble :: Int -> Int
luhnDouble n = if double <= 9 then double else double - 9
            where double = n * 2

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = (luhnDouble a + b + luhnDouble c + d) `mod` 10 == 0
