module Fib where

solve :: IO ()
solve = do
    -- File read can be awkward
    line <- readFile "rosalind_fib.txt"
    let solution = (read <$> words line) :: [Int]
    print solution


-- kSequence :: Int -> [Int] 
-- kSequence
-- kSequence k = k : kSequence k'