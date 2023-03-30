
data Colors = Red | White | Golden deriving Show 

data MobileDevice = Smartphone Colors | Laptop Colors| Tablet Colors Int
                     deriving Show


descriere :: MobileDevice -> String
descriere (Laptop col)              = "laptop culoare " ++ show(col)
descriere (Tablet col i)         = "tableta culoare " ++ show(col) ++ " de dim " ++ show(i)
descriere (Smartphone col)          = "telefon mobil culoare"++ show(col)


-- ex. 2

data Arb = Null | Frunza Integer | Nod Integer Arb Arb deriving (Show, Eq)

--funtia auxialia care testeaza daca toate elem din arbore < sau > decat x

isSmaller :: Arb -> Integer -> Bool
isSmaller (Frunza x) i      = (i <= x)
isSmaller (Nod x arbl arbr) i = (i<=x) && (isSmaller arbl i)  && (isSmaller arbr i)

isGreater :: Arb -> Integer -> Bool
isGreater (Frunza x) i      = (i >= x)
isGreater (Nod x arbl arbr) i = (i >=x ) && (isGreater arbl i)  && (isGreater arbr i)

isBST :: Arb -> Bool
isBST (Frunza i) = True
isBST (Nod i arbl arbr) = (isGreater arbl i) 
                        && (isSmaller arbr i)
                        &&isBST arbl && isBST arbr


--inserare element in arbore binar de cautare
bSearch :: Arb ->Integer-> Bool 
bSearch (Frunza x) i = i == x
bSearch (Nod x arbl arbr) i | i == x = True 
bSearch  (Nod x arbl arbr) i | i < x = bSearch arbl i
bSearch (Nod x arbl  arbr) i | i > x = bSearch arbr i



--Scriet, i o funct, ie care insereaz ̆a o valoare de tip ˆıntreg ˆıntr-un arbore binar de cautare.

insertNode :: Integer -> Arb -> Arb 
insertNode val (Frunza x) | val >= x               = Nod x Null (Frunza val)
insertNode val (Frunza x) | val < x                = Nod x (Frunza val) Null 
insertNode val (Nod x arbl arbr) | val >= x        = Nod x arbl (insertNode val arbr)
insertNode val (Nod x arbl arbr) | val < x         = Nod x (insertNode val arbl) arbr



data Arb' = Leaf Integer | Node Integer Arb' Arb' deriving (Show, Eq)
insert :: Integer -> Arb' -> Arb'
insert val (Leaf x) = if val < x then Node x (Leaf val) (Leaf x) else Node x (Leaf x) (Leaf val)
insert val (Node x left right)
  | val == x = Node x left right
  | val < x = Node x (insert val left) right
  | otherwise = Node x left (insert val right)