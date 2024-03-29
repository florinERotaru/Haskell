and1 :: Bool -> Bool -> Bool
and1 False _ = False
and1 _ False = False
and1 _ _ = True

--2
hasDivisors :: Integer -> Integer -> Integer -> Bool;
hasDivisors n a b | a > b           = False
hasDivisors n a b | n `mod` a == 0  = True
hasDivisors n a b                   = hasDivisors n (a+1) b
 

--2 Primality Test

isPrime :: Integer -> Bool;
--isPrime n = not (hasDivisors n 2 (round (sqrt (fromIntegral n))))
isPrime n = not (hasDivisors n 2 (n - 1))



--3 cmmdc euclid prin scaderi

subtractionGCD :: Integer -> Integer -> Integer;
subtractionGCD a b | a == b     = a
subtractionGCD a b | a > b      = subtractionGCD (a - b) b
                   | otherwise    = subtractionGCD a (b-a)




--3 cmmdc euclid prin impartiri


divGCD :: Integer -> Integer -> Integer;
divGCD a 0                       = a 
divGCD 0 b                       = b
divGCD a b | a < b               = divGCD b a
           | otherwise           = divGCD b  (a `mod` b)



--3 cmmd binar


binGCD :: Integer -> Integer -> Integer;
binGCD a 0                          = a 
binGCD 0 b                          = b
binGCD a b  | a `mod` 2 == 0
              && 
              b `mod` 2 == 0        = 2 * binGCD (a `div` 2) (b `div` 2)
            
            | a `mod` 2 == 0        = binGCD (a `div` 2) b
            | b `mod` 2 == 0        = binGCD a (b `div` 2)
            | otherwise             = binGCD (abs  (a - b) )  (min a b)

        
--4 
--i'th element of fib series
fib :: Int -> Int;
fib 0 = 0
fib 1 = 1
fib n | n == 0          = 0
      | otherwise       = fib ( n - 1 ) + fib ( n - 2 )
            
-- fib cu acumulatori

fibonaux :: Integer -> Integer -> Integer -> Integer;
fibonaux 0 a _          = a 
fibonaux n a b          = fibonaux (n - 1) b (a + b)


fibo' ::  Integer -> Integer;
fibo' n         = fibonaux n 0 1


efibo :: Integer -> Integer
efibo x | x ==0 || x ==1 = x
        |otherwise       = efibo(x-1) + efibo(x-2)


efiboaux :: Integer -> Integer -> Integer -> Integer 
efiboaux 1 a _ = a -- because we give 2 first numbers already
efiboaux n a b = efiboaux (n-1) (a+b) a

-- C.B 
--       efiboaux 1 a b = a = b + fib(n-2) = fib(n)
--       n - initial param
-- C.I   
      

--       proove that
--       efiboaux n 1 0 = fib (n) 
succc :: Integer -> Integer
succc x | x<0 = error "no"

mymod :: Integer -> Integer -> Integer
mymod nr div | nr < div = nr
             |otherwise = mymod (nr - div) div


mydiv :: Integer -> Integer -> Integer
mydiv nr div | nr < div = 0
             |otherwise = 1 + mydiv (nr - div) div


testss :: [Char] -> Integer
testss [] = 0
testss (hd:tl) = 1 + testss tl
