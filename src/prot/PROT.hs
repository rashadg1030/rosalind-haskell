{-# LANGUAGE OverloadedStrings #-}

module PROT where

import Data.Text (Text)

import qualified Data.Text as T
import qualified Data.Text.IO as T

solve :: IO ()
solve = do
    (rna:_) <- T.lines <$> T.readFile "rosalind_prot.txt"
    let solution = rnaToProt rna
    T.writeFile "prot_solution.txt" solution

-- UUU F      CUU L      AUU I      GUU V
-- UUC F      CUC L      AUC I      GUC V
-- UUA L      CUA L      AUA I      GUA V
-- UUG L      CUG L      AUG M      GUG V
-- UCU S      CCU P      ACU T      GCU A
-- UCC S      CCC P      ACC T      GCC A
-- UCA S      CCA P      ACA T      GCA A
-- UCG S      CCG P      ACG T      GCG A
-- UAU Y      CAU H      AAU N      GAU D
-- UAC Y      CAC H      AAC N      GAC D
-- UAA Stop   CAA Q      AAA K      GAA E
-- UAG Stop   CAG Q      AAG K      GAG E
-- UGU C      CGU R      AGU S      GGU G
-- UGC C      CGC R      AGC S      GGC G
-- UGA Stop   CGA R      AGA R      GGA G
-- UGG W      CGG R      AGG R      GGG G

rnaToProt :: Text -> Text
rnaToProt ""  = ""
rnaToProt rna = case T.take 3 rna of
                    "UUU" -> addAmino "F"
                    "UUC" -> addAmino "F"
                    "UUA" -> addAmino "L"
                    "UUG" -> addAmino "L"
                    "UCU" -> addAmino "S"
                    "UCC" -> addAmino "S"
                    "UCA" -> addAmino "S"
                    "UCG" -> addAmino "S"
                    "UAU" -> addAmino "Y"
                    "UAC" -> addAmino "Y"
                    "UAA" -> ""
                    "UAG" -> ""
                    "UGU" -> addAmino "C"
                    "UGC" -> addAmino "C"
                    "UGA" -> ""
                    "UGG" -> addAmino "W"
                    "CUU" -> addAmino "L"
                    "CUC" -> addAmino "L"
                    "CUA" -> addAmino "L"
                    "CUG" -> addAmino "L"
                    "CCC" -> addAmino "P"
                    "CCU" -> addAmino "P"
                    "CCA" -> addAmino "P"
                    "CCG" -> addAmino "P"
                    "CAU" -> addAmino "H"
                    "CAC" -> addAmino "H"
                    "CAA" -> addAmino "Q"
                    "CAG" -> addAmino "Q"
                    "CGU" -> addAmino "R"
                    "CGC" -> addAmino "R"
                    "CGA" -> addAmino "R"
                    "CGG" -> addAmino "R"
                    "AUU" -> addAmino "I"
                    "AUC" -> addAmino "I"
                    "AUA" -> addAmino "I"
                    "AUG" -> addAmino "M"
                    "ACU" -> addAmino "T"
                    "ACC" -> addAmino "T"
                    "ACA" -> addAmino "T"
                    "ACG" -> addAmino "T"
                    "AAU" -> addAmino "N"
                    "AAC" -> addAmino "N"
                    "AAA" -> addAmino "K"
                    "AAG" -> addAmino "K"
                    "AGU" -> addAmino "S"
                    "AGC" -> addAmino "S"
                    "AGA" -> addAmino "R"
                    "AGG" -> addAmino "R"
                    "GUU" -> addAmino "V"
                    "GUC" -> addAmino "V"
                    "GUA" -> addAmino "V"
                    "GUG" -> addAmino "V"
                    "GCU" -> addAmino "A"
                    "GCC" -> addAmino "A"
                    "GCA" -> addAmino "A"
                    "GCG" -> addAmino "A"
                    "GAU" -> addAmino "D"
                    "GAC" -> addAmino "D"
                    "GAA" -> addAmino "E"
                    "GAG" -> addAmino "E"
                    "GGU" -> addAmino "G"
                    "GGC" -> addAmino "G"
                    "GGA" -> addAmino "G"
                    "GGG" -> addAmino "G"
  where
    addAmino :: Text -> Text
    addAmino aa = aa `T.append` (rnaToProt $ T.drop 3 rna)
