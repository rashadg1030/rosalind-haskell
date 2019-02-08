module REVC where

solve :: IO ()
solve = do
    dna <- readFile "rosalind_revc.txt"
    let solution = (compliment . reverse) dna 
    writeFile "revc_solution.txt" solution

compliment :: String -> String
compliment ""     = ""
compliment (n:ns)
    | n == 'A'  = 'T' : compliment ns
    | n == 'C'  = 'G' : compliment ns
    | n == 'G'  = 'C' : compliment ns
    | n == 'T'  = 'A' : compliment ns
    | otherwise = n : compliment ns