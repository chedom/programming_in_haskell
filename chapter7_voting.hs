count :: Eq a => a -> [a] -> Int
count x  = length . filter (== x)

