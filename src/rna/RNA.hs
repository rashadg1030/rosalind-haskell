module RNA where

solve :: IO ()
solve = do
    dna <- readFile "rosalind_rna.txt"
    let solution = transcribe dna 
    writeFile "rna_solution.txt" solution

transcribe :: String -> String
transcribe ""     = ""
transcribe (n:ns)
    | n == 'T'  = 'U' : transcribe ns
    | otherwise = n : transcribe ns