--1
putStr' :: String -> IO ()
putStr' xs = sequence_ [putChar x | x <- xs]

--4
adder :: IO ()
adder = do putStr "How many numbers?"
           count <- getLine
           total <- getTotal 0 (read count :: Int)
           putStr "The total is "
           putStrLn (show total)

getTotal :: Int -> Int -> IO Int
getTotal total 0 = return total
getTotal total attempt = do n <- getLine
                            getTotal (total + (read n :: Int)) (attempt - 1)


