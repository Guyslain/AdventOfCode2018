root :: Int -> Int
root n =
  go 1 n
  where
    go l u
      | l + 1 == u = l
      | otherwise =
        let m = (l + u) `div` 2 in
        if m * m > n then go l m
        else go m u

primes :: Int -> [ Int ]
primes n =
  reverse . foldr consIfPrime [] $ reverse [2..n]

consIfPrime k primes =
  if all (\p -> (k `rem` p) /= 0) primes then k:primes
  else primes
      

primeDivisors :: Int -> [ Int ]
primeDivisors n =
  if bigPrime == 1 then smallPrimes
  else bigPrime:smallPrimes
  where
    bigPrime = foldr removePrime n smallPrimes
    smallPrimes = [ d | d <- primes (root n), n `rem` d == 0]
    removePrime p n
      | n `rem` p == 0 = removePrime p (n `div` p)
      | otherwise = n

sublists :: [a] -> [[a]]
sublists [] = [[]]
sublists (head:tail) = concat . map (\l -> [l, head:l]) $ sublists tail


divisors :: Int -> [ Int ]
divisors =
  map product . sublists . primeDivisors
