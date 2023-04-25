module Day1part1 (process) where 

readSignedInt :: String -> Int
readSignedInt ('+':int) = read int
readSignedInt ('-':int) = - read int

process :: String -> String
process  = show . sum . map readSignedInt . lines
