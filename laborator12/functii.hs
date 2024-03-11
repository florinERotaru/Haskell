type Id = String
data Term = Var Id
    | App Term Term
    | Lambda Id Term deriving (Show, Eq)

reduce1' :: Term -> [Id] -> Maybe Term
reduce1' (Var id') _ = Nothing
reduce1' (App (Lambda id term) term') avoid =
Just (casubst id term' term avoid)
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
