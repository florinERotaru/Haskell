type Id = String

--lambda terms

data Term = Var Id
     | App Term Term 
     | Lambda Id Term deriving (Show, Eq)

--0.1 scrieti o valoare de tip Term care reprezina λx.λy.x

subst :: Id -> Term -> Term -> Term 
subst id term (Var id') = term 
subst id term (App term1 term2) = App (subst id term term1) (subst id term term2)
subst id term (Lambda id' term') | id == id' = Lambda id term'
                                 | otherwise = Lambda id' (subst id term term')


remove :: Id -> [Id] -> [Id]
remove _ [] = []
remove id (hd:tl) | id == hd = remove id tl
                  | True = [hd] ++ (remove id tl)


free :: Term -> [Id]
free (Var id) = [id]
free (App term1 term2) = free term1 ++ free term2
free (Lambda id term) = remove id (free term)


vars :: Term -> [Id]
vars (Var id) = [id]
vars (App term1 term2) = vars term1 ++ vars term2 
vars (Lambda id term) = vars term ++[id]


fresh' :: [Id] -> Int -> Id
fresh' ids index = if ("n" ++ (show index)) `elem` ids then fresh' ids (index + 1)
                    else "n" ++ (show index)
fresh :: [Id] -> Id
fresh ids = fresh' ids 0

casubst :: Id -> Term -> Term -> [Id] -> Term
casubst id term (Var id') _ | id == id' = term
                            | True      = (Var id')

casubst id term (App term1 term2) avoid = App (casubst id term term1 avoid) (casubst id term term2 avoid)
casubst id term (Lambda id' term') avoid | id == id' = (Lambda id' term')
                                         | id' `elem` (free term) =
                                              let id'' = fresh avoid in
                                                   Lambda id'' (casubst id term (subst id' (Var id'') term')  (id : avoid))
                                         | True = Lambda id' (casubst id term term' (id : avoid))

reduce1' :: Term -> [Id] -> Maybe Term
reduce1' (Var id') _ = Nothing
reduce1' (App (Lambda id term) term') avoid = Just (casubst id term' term avoid)
reduce1' (App term1 term2) avoid = case reduce1' term1 avoid of
     Nothing -> case reduce1' term2 avoid of
                    Nothing -> Nothing
                    Just term2' -> Just (App term1 term2')
     Just term1' -> Just (App term1' term2)
reduce1' (Lambda id term) avoid = case reduce1' term avoid of
     Nothing -> Nothing
     Just term' -> Just (Lambda id term')


reduce1 :: Term -> Maybe Term
reduce1 t = reduce1' t (vars t)


reduce :: Term -> Term
reduce term = case reduce1 term of
               Nothing -> term
               Just term' -> reduce term'
--BOOLEAN
trueVal :: Term 
trueVal = (Lambda "x" (Lambda "y" (Var "x")))

falseVal :: Term 
falseVal = (Lambda "x" (Lambda "y" (Var "y")))


-- BOOLEAN OPERATIONS
andd ::Term -> Term -> Term 
andd term1 term2 = reduce (App (App ( Lambda "u" (Lambda "v" (App (App (Var "u") (Var "v")) (Var "u")))) term1) term2)

-- or ::Term -> Term -> Maybe Term 
-- or term1 term2 = reduce (App (App ( Lambda "u" (Lambda "v" (App (App (Var "u") (Var "u")) (Var "v")))) term1) term2)


-- (Lambda "u" (Lambda "v" (App (App (App (Var "u") (Var "v")) Var "u"))))

-- Lambda "v"    (App (App (Var "u") (Var "v")) (Var "u"))

-- 0.2 

-- reduce (App (Lambda "x" (Var "x1")) (App (Lambda "x2" (Var "x2")) (Lambda "z" (App (Lambda "x3" (Var "x3")) (Var "z")))))
-- Var "x1"


-- (App (Lambda "x1" (Lambda "x2" (Var "x2"))) (App (Lambda "x" (Var "x")) (Lambda "y" (Var "y"))))

--0.3 

callbyname' :: Term -> [Id] -> Maybe Term
callbyname' (Var id') _ = Nothing
callbyname' (App (Lambda id term) term') avoid = Just (casubst id term' term avoid)
callbyname' (App term1 term2) avoid = case reduce1' term1 avoid of
     Nothing -> case reduce1' term2 avoid of
                    Nothing -> Nothing
                    Just term2' -> Just (App term1 term2')
     Just term1' -> Just (App term1' term2)
callbyname' (Lambda id term) avoid = Nothing


callbyname :: Term -> Maybe Term
callbyname t = callbyname' t (vars t)



redCBN :: Term -> Term
-- redCBN  (App (Lambda smth (App (Lambda id term) term')) other) = (App (Lambda smth (App (Lambda id term) term')) other)
redCBN term = case callbyname term of
               Nothing -> term
               Just term' -> redCBN term'


-- App (Lambda "x1" (Var "x1")) (App (Lambda "x2" (Var "x2")) (Lambda "z" (App (Lambda "x3" (Var "x3")) (Var "z"))))


strategy1' :: Term -> [Id] -> [Term]
strategy1' (Var _) _ = []
strategy1' (App (Lambda id term) term') avoid = [casubst id term' term avoid] ++
     let all = strategy1' term avoid in
     let all' = strategy1' term' avoid in
     [ App (Lambda id successorTerm) term' | successorTerm <- all ] ++
     [ App (Lambda id term) successorTerm' | successorTerm' <- all']
strategy1' (App term1 term2) avoid =
     let all1 = strategy1' term1 avoid in
     let all2 = strategy1' term2 avoid in
     [ App sterm1 term2 | sterm1 <- all1 ] ++
     [ App term1 sterm2 | sterm2 <- all2 ]
strategy1' (Lambda id term) avoid =
     let all = strategy1' term avoid in
     [ Lambda id sterm | sterm <- all ]


strategy1 :: Term -> [Term]
strategy1 term = strategy1' term (vars term)


strategy :: Term -> [Term]
strategy term = let all = strategy1 term in case all of
     [] -> [term]
     _ -> concat (map strategy all)




callbyvalue' :: Term -> [Id] -> Maybe Term
callbyvalue' (Var id') _ = Nothing
callbyvalue' (App (Lambda id term) (App smth1 smth2) ) avoid = Nothing
callbyvalue' (App (Lambda id term) term' ) avoid             = Just (casubst id term' term avoid)

callbyvalue' (App term1 term2) avoid = case reduce1' term1 avoid of
     Nothing -> case reduce1' term2 avoid of
                    Nothing -> Nothing
                    Just term2' -> Just (App term1 term2')
     Just term1' -> Just (App term1' term2)
callbyvalue' (Lambda id term) avoid = Nothing


callbyvalue :: Term -> Maybe Term
callbyvalue t = callbyname' t (vars t)



redCBV :: Term -> Term
-- redCBN  (App (Lambda smth (App (Lambda id term) term')) other) = (App (Lambda smth (App (Lambda id term) term')) other)
redCBV term = case callbyvalue term of
               Nothing -> term
               Just term' -> redCBV term'



myhead :: [a] -> Maybe a
myhead = foldr (\x _ -> Just x) Nothing



-- data Exp = Val Int 
--           | Prod (Exp) (Exp)
--           | Sum (Exp) (Exp) deriving Show


data MyType a = Val a deriving Show

example :: MyType a
example = Val x

          
