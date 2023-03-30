-- currying


f :: (Int, Int) -> Int 
f (x, y) = x + y

g :: Int -> Int -> Int
g x y = x + y


-- varianta curried pentru 

    -- addThree :: (Int, Int, Int) -> Int
    -- addThree (x,y,z) = x + y + z

-- ex1 
addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

--variabila de tip

process :: (a -> a) -> a -> a 
process f x = f x


--2.1 

suma :: (Int -> Int) -> Int -> Int -> Int 
suma f x1 x2 | x1 > x2      = 0
suma f x1 x2 | x1 <= x2       = f x1 + suma f (x1+1) x2 

-- 3, 4, 5, 6
-- 4, 5, 6, 7
--22


--2.2
--Scriet, i o funct, ie care returneaz ̆a compunerea a dou ̆a functii

composition :: (Int -> Int) -> (Int -> Int) -> (Int -> Int) 
composition f g = \ x -> f (g x)


--2.3 scriet o functio care primeste o lista de functii si 
--returneaza compunerea lor
compositionList :: [(Float -> Float)] -> Float -> Float
compositionList [] = \ x -> x
compositionList (hd : tl) = \ x -> hd ((compositionList tl) x)

--2.4 suma elementelor dintr-o lista
reduce :: Int -> (Int -> Int -> Int) -> [Int] -> Int 
reduce x _ [] = x
reduce x f (hd : tl) = f hd (reduce x f tl)

--2.5 scrieti o functie care aplica fiecarui element al unei list
--o functie si returneaza lista obtinuta

mapp :: (Int -> Int) -> [Int] -> [Int]
mapp _ [] = []
mapp f (hd:tl) = (f hd : mapp f tl)

--2.6 Scrieti o functie care va returna lista elementelor pentru o
--care o functie de tipul a -> Bool returneaza true

filterr :: (a -> Bool) -> [a] -> [a]
filterr _ [] = []
filterr predicate (hd:tl) | predicate hd == True = (hd : (filterr predicate tl))
filterr predicate (hd:tl)                        = filterr predicate tl


--2.7  
-- Scriet, i o funct, ie care implementeaz ̆a
-- comportamentul fold (foldr, foldl)
-- pe lista definit ̆a ˆın laboratorul anterior


myFoldl :: (a -> a -> a) -> a -> [a] -> a 
myFoldl f x [] = x
myFoldl f x (hd : tl) = myFoldl f (f x hd) tl 
-- Func(f 64  [4,2,4]) = func ( f 64/4 [2,4]) = func 16 [2, 4]  =  func 16/2 [4] = func 8 [4] = func 8/4 nil = func 2 nil  =  




myFoldr :: (a -> a -> a) -> a -> [a] -> a 
myFoldr f x [] = x
myFoldr f x (hd : tl) = f x (myFoldr f x tl)


--in: radacina, f
--aplica in preordine (rsd)

data Tree = Null | Node Int Tree Tree deriving Show

preorder :: (Int -> Int) -> Tree -> Tree 
preorder f Null = Null
preorder f (Node x left right) = Node (f x) (preorder f left) (preorder f right)

showpreorder :: Tree -> [Int]
showpreorder Null = []
showpreorder (Node x left right) = ([x] ++ showpreorder left ++ showpreorder right)




showinorder :: Tree -> [Int]
showinorder Null = []
showinorder (Node x left right) = (showinorder left ++ [x] ++  showinorder right)