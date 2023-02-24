id2 x = x

--specifying the type
sumThree :: Int -> Int -> Int -> Int
sumThree x y z = x + y + z


--maximum out of 2 numbers
myMax :: Int -> Int -> Int 
myMax x y = if x <= y then y else x

--maximum out of 3 numbers
myMax3 :: Int->Int->Int->Int
myMax3 x y z = if (x >= y) && (x >= z) then x
        else   if (y >= z) then y
        else   z


-- $recursive functions

--sum from 1 to n
mySum :: Int -> Int
mySum n = 
    if (n <= 0 )
        then 0
    else
        n+mySum (n-1)


--i'th element of fib series
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = if (n<0) 
            then 0
        else
            fib (n-1) + fib (n-2)


--gcd 

gcd2 :: Int->Int->Int
gcd2 x 0 = x
gcd2 x y = gcd y (mod x y)
