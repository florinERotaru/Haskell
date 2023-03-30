--Creat, i instant, e pentru Nat ale claselor Eq, Ord, Integral s, i Num


data Nat = Cons [Bool]-- lista de valori de tip boolean

instance Eq Nat where 
    (==) (Cons []) (Cons []) = True
    (==) (Cons (hd1:tl1)) (Cons (hd2:tl2)) = ((==) hd1 hd2) && ((==) (Cons tl1) (Cons tl2))


instance Ord Nat where
    (<=) (Cons []) (Cons []) = True
    (<=) (Cons (hd1:tl1)) (Cons (hd2:tl2)) = (hd1 <= hd2) || ((<=) (Cons tl1) (Cons tl2))




instance Integral Nat where 
    toInteger elt = toInteger (fromEnum elt)


convertToBinary :: Int -> [Bool]
convertToBinary 0                  = []
convertToBinary x | x `mod` 2 == 1 = convertToBinary (x `div` 2) ++ [True]
convertToBinary x | x `mod` 2 == 0 = convertToBinary (x `div` 2) ++ [False]


instance Enum Nat where
    toEnum x = Cons (convertToBinary x)
    fromEnum (Cons []) = 0
    fromEnum (Cons (hd:tl)) | hd == True = 2 ^ (length tl) + fromEnum (Cons tl)
                            | hd == False = fromEnum (Cons tl) 


binarySum Bool -> Bool -> [Bool]
binarySum True True     = [True, False]
binarySum True False    = [True]
binarySum False True    = [True]
binarySum False False   = [False]

binarySumVec :: [Bool] -> [Bool] -> [Bool]
binarySum (hd1:tl1) (hd2:tl2) = 

--


instance Real Nat where
    (+) (Cons bin1) (Cons bin2)