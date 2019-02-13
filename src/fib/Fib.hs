module Fib where

data Rabbit = Adult | Kitten
    deriving (Show, Eq)

tick :: Int -> Rabbit -> [Rabbit]
tick k Adult  = [Adult] ++ replicate k Kitten
tick k Kitten = [Adult] 

tock :: [Rabbit] -> Int -> Int -> [Rabbit]
tock [] _ _     = []
tock rs n b
    | n == 1    = rs
    | otherwise = tock (concatMap (tick b) rs) (n-1) b
    
initFarm :: (Int, Int) -> [Rabbit]
initFarm = uncurry $ tock [Kitten]

awkward :: [Int] -> (Int,Int)
awkward [x,y] = (x,y)
awkward _     = (0,0)

solve :: IO ()
solve = do
    -- File read can be awkward
    line <- readFile "rosalind_fib.txt"
    let solution = length $ initFarm $ awkward $ read <$> words line  
    writeFile "fib_solution.txt" (show solution) 


kSequence :: [Int]
kSequence = 0 : 3 : rem kSequence
  where
    rem (a : t@(b:_)) = (a+b) : rem t