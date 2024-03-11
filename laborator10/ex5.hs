-- Proiectati un Zipper pentru arbori in care nodurile au un numar arbitrar de subarbori (nu doar doi: stanga si dreapta). Hint 1: 

import Prelude hiding (Left, Right)

data Tree = Node Int [Tree] deriving (Show, Eq)

data Dir = Down | Right deriving (Show, Eq)

t = Node 10 [Node 3 [], Node 4 [Node 1 [], Node 2 [], Node 22 [], Node 33[] ], Node 5 [], Node 6 []]

atPos :: [Tree] -> [Dir] -> Maybe Int
atPos ((Node v subtrees) : tl) [] = v
atPos (hd:right_trees) (Right:dirs)         = atPos right_trees dirs 
atPos ((Node v subtrees) : right_trees) (Down:dirs) = atPos subtrees dirs

data crumbs = 