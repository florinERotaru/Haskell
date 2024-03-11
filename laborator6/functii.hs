--1. Definit, i o funct, ie care ˆıntoarce minimul unei liste, calculˆand primul element din lista
--sortata
import Data.List

genList ::Int -> [Int]
genList x | x>100000 = []
genList x = genList (x+1) ++ [(13 * x*x+1) `mod` 97]

min1 :: [Int] -> Int
min1 (hd:tl) = sort (hd:tl) !! 0

sortExperiment :: [Int] -> Int 
sortExperiment [] = 0
sortExperiment (hd : tl) = hd + sortExperiment(tl)
--2Proiectat, i un experiment computat, ional prin care s ̆a comparat, 
--i complexitatea-timp a
--funct, iei minim cu complexitatea-timp a funct, iei de sortare

-- genList 1
    -- [21,53,14,1,14,53,21,15,35,81,56,57,84,40,22,30,64,27,16,31,72,42,38,60,11,85,88,20,75,59,69,8,70,61,78,24,93,91,18,68,47,52,83,43,29,41,79,46,39,58,6,77,77,6,58,39,46,79,41,29,43,83,52,47,68,18,91,93,24,78,61,70,8,69,59,75,20,88,85,11,60,38,42,72,31,16,27,64,30,22,40,84,57,56,81,35,15,21,53,14. ....]
    
    -- > :set +s 
    -- > min1 genList 1 = 0 in 0.98s
    -- > sum (sort genList 1 sorted) in 0.97s

            --insertion sort
findPlace :: Int ->[Int] -> [Int] --assuming the list is sorted.
findPlace x []                   = [x]
findPlace x (hd:tl) | hd > x     = (x : hd : tl)
                    | otherwise  = (hd : (findPlace x tl))

--[1,2,3,10,11] <- 6
insSort :: [Int] -> [Int]
insSort [] = [] 
insSort (hd:tl) = findPlace hd (insSort tl)


findPlace 1 (call(2,3,10,11, 3))
findPlace 1 (findPlace 2 (call ))

