import Data.Char

type Bit = Int

bin2int :: [Bit] -> Int
bin2int bits = sum [coef * weight | (coef, weight) <- zip bits weights]
                    where weights = iterate (*2) 1

--bin2int = foldr (\x y = x + 2*y) 0

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)
 
make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

encode :: String -> [Bit]
encode = concat . map (addParityBit .  make8 . int2bin . ord)

chop9 :: [Bit] -> [[Bit]]
chop9 [] = []
chop9 bits = take 9 bits : chop9 (drop 9 bits)

decode :: [Bit] -> String
decode = map (chr . bin2int . deleteParityBit ) . chop9

transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
--channel = id
channel = tail

computeParityBit :: [Bit] -> Bit
computeParityBit xs = sum (filter (== 1) xs) `mod` 2 

addParityBit :: [Bit] -> [Bit]
addParityBit xs = xs ++ [computeParityBit xs]

checkParityBit :: [Bit] -> Bool
checkParityBit xs = last xs == computeParityBit (take 8 xs)

deleteParityBit :: [Bit] -> [Bit]
deleteParityBit xs = if checkParityBit xs
                        then init xs
                        else error "Incorrect parity bit"

