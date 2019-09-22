{-# LANGUAGE OverloadedStrings #-}

module SUBS where

import Data.Text (Text)

import qualified Data.Text as T
import qualified Data.Text.IO as T

solve :: IO ()
solve = do
    (s:t:_) <- T.lines <$> T.readFile "rosalind_subs.txt"
    let solution = findMotif 1 [] s t
    T.writeFile "subs_solution.txt" (T.intercalate " " (T.pack . show <$> solution))

-- | t is a substring of t.
findMotif :: Int      -- | Current position
          -> [Int]    -- | List of locations of motifs
          -> Text     -- | DNA String
          -> Text     -- | Motif (substring)
          -> [Int]    -- | New list of locations of motifs
findMotif pos locs s t
    | T.null s  = locs
    | otherwise = case possibleMotif == t of
                    False -> findMotif (pos + 1) locs restOfDNA t
                    True  -> findMotif (pos + 1) (locs ++ [pos]) restOfDNA t
  where
    lengthOfSub :: Int
    lengthOfSub = T.length t

    possibleMotif :: Text
    possibleMotif = T.take lengthOfSub s

    restOfDNA :: Text
    restOfDNA = T.drop 1 s
