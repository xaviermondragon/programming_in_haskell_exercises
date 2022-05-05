import Data.Char

-- We declare a type for bits as a synonym for the type of integers
type Bit = Int

bin2int :: [Bit] -> Int
bin2int = foldr (\x y -> x + 2*y) 0


int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)


make9 :: [Bit] -> [Bit]
make9 bits = addparitybit (take 8 (bits ++ repeat 0))


addparitybit :: [Bit] -> [Bit]
addparitybit bits = ((sum bits) `mod` 2) : bits

encode :: String -> [Bit]
encode = concat . map (make9 . int2bin . ord)


chop9 :: [Bit] -> [[Bit]]
chop9 []   = []
chop9 bits = take 9 bits : chop9 (drop 9 bits)


checkparitybit :: [Bit] -> [Bit]
checkparitybit (bit:bits) | ((sum bits) `mod` 2) == bit =bits 
                          | otherwise = error("Parity error.")

decode :: [Bit] -> String
decode = map (chr . bin2int . checkparitybit) . chop9

transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = id

transmitcorrupt :: String -> String
transmitcorrupt = decode . tail . encode