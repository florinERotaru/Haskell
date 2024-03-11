data Lista = Nil | Cons Int Lista deriving (Show, Eq)

l = Cons 3 (Cons 8 (Cons 13 (Cons 7 Nil)))

data Dir = Fwd deriving (Show, Eq)

data Crumb = Go Int deriving (Show, Eq)

goFwd :: (Lista, [Crumb]) ->Maybe (Lista, [Crumb])
goFwd (Nil, crumbs)       = Nothing
goFwd (Cons v tl, crumbs) = Just (tl, Go v : crumbs)

chg :: Int -> (Lista, [Crumb]) -> Maybe (Lista, [Crumb])
chg v' (Nil, crumbs)       = Nothing
chg v' (Cons v tl, crumbs) = Just (Cons v' tl, crumbs)

goBwd :: (Lista, [Crumb]) -> Maybe (Lista, [Crumb])
goBwd (_, [])            = Nothing
goBwd (l, Go v : crumbs) = Just (Cons v l, crumbs)

type Zipper = (Lista, [Crumb])

(-:) :: Zipper -> (Zipper -> Zipper) -> Zipper
z -: f = f z

