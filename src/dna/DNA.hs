module DNA where

solve :: IO ()
solve = do
    dna <- readFile "rosalind_dna.txt"
    let count = countNucleotides (0,0,0,0) dna 
    dnaCountToFile count

dnaCountToFile :: (Int, Int, Int, Int) -> IO ()
dnaCountToFile (a, c, g, t) = do
    writeFile "dna_solution.txt" solution
    where 
        solution = show a ++ " " ++ show c ++ " " ++ show g ++ " " ++ show t  

countNucleotides :: (Int, Int, Int, Int) -> String -> (Int, Int, Int, Int)
countNucleotides count ""                  = count
countNucleotides count@(a, c, g, t) (n:ns) 
        | n == 'A'  = countNucleotides (a+1, c, g, t) ns
        | n == 'C'  = countNucleotides (a, c+1, g, t) ns
        | n == 'G'  = countNucleotides (a, c, g+1, t) ns
        | n == 'T'  = countNucleotides (a, c, g, t+1) ns
        | otherwise = countNucleotides (a, c, g, t) ns