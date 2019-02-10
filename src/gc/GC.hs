{-# LANGUAGE FlexibleInstances #-}

module GC where

import Data.List
import Data.Tuple

solve :: IO ()
solve = do
    fasta <- readFile "rosalind_gc.txt"
    let solution = writeFunc $ someOther $ someFunc . lines $ fasta
    writeFile "gc_solution.txt" solution

writeFunc :: (String, Float) -> String
writeFunc (s,f) = drop 1 s ++ "\n" ++ show f 

someOther :: [(String, Float)] -> (String, Float)
someOther = swap . maximum . (swap <$>) 

someFunc :: [String] -> [(String, Float)]
someFunc []        = []                    
someFunc (line:ls) = if isLabel line then 
                        (line, gcContent (concat $ takeWhile (not . isLabel) ls) 0.0 0.0) : (someFunc $ dropWhile (not . isLabel) ls) 
                     else
                        [] ++ someFunc ls  
    where
        isLabel :: String -> Bool
        isLabel ""     = False
        isLabel (c:cs) = c == '>'


gcContent :: String -> Float -> Float -> Float
gcContent [] gc total     = (gc/total) * 100.0
gcContent (n:ns) gc total
    | n == 'C' || n == 'G' = gcContent ns (gc+1.0) (total+1.0)
    | n == 'A' || n == 'T' = gcContent ns gc (total+1.0)