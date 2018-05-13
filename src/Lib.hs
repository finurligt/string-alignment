module Lib
    ( outputOptAlignments, similarityScore, maximaBy, optAlignments, scoreAlignment, allAlignments, attachHeads, similarityScore'
    ) where

import Control.Arrow

scoreMatch = 0
scoreMismatch = -1
scoreSpace = -1

attachHeads :: a -> a -> [([a],[a])] -> [([a],[a])]
attachHeads h1 h2 aList = [(h1:xs,h2:ys) | (xs,ys) <- aList]

similarityScore :: String -> String -> Int
similarityScore (x:xs) [] = similarityScore xs [] + scoreSpace
similarityScore [] (y:ys) =  similarityScore ys [] + scoreSpace
similarityScore [] [] = 0
similarityScore (x:xs) (y:ys) = maximum [similarityScore xs ys + if x == y then scoreMatch else scoreMismatch,
                                      similarityScore xs (y:ys) + scoreSpace,
                                      similarityScore (x:xs) ys + scoreSpace ]

maximaBy :: Ord b => (a -> b) -> [a] -> [a]
maximaBy f = foldl (addIfBigger f) []


addIfBigger :: Ord b => (a -> b) -> [a] -> a -> [a]
addIfBigger _ [] x = [x]
addIfBigger f (x:xs) e
  | f x < f e = [e]
  | f x == f e = e:x:xs
  | otherwise = x:xs

type AlignmentType = (String, String)

optAlignments :: String -> String -> [AlignmentType]
optAlignments string1 string2 = maximaBy scoreAlignment $ allAlignments string1 string2

scoreAlignment :: AlignmentType -> Int
scoreAlignment (x:xs,y:ys)
  | x == y = scoreAlignment (xs,ys) + scoreMatch
  | (x == '-') || (y == '-') = scoreAlignment (xs,ys) + scoreSpace
  | otherwise = scoreAlignment (xs,ys) + scoreMismatch
scoreAlignment _ = 0

allAlignments :: String -> String -> [AlignmentType]
allAlignments (x:xs) (y:ys) = attachHeads x y (allAlignments xs ys) ++ attachHeads x '-' (allAlignments xs (y:ys)) ++ attachHeads '-' y (allAlignments (x:xs) ys)
allAlignments [] [] = [([],[])]
allAlignments [] (y:ys) = attachHeads '-' y $ allAlignments [] ys
allAlignments (x:xs) [] = attachHeads x '-' $ allAlignments xs []

outputOptAlignments :: String -> String -> IO ()
outputOptAlignments =
  ((\l -> do
    putStrLn $ "There are " ++ show (length l) ++ " optimal alignments:"
    mapM_ (\(string1, string2) -> putStrLn $ '\n':string1 ++ '\n':string2) l
  ).) . optAlignments

similarityScore' :: String -> String -> Int
similarityScore' xs ys = simScore (length xs) (length ys)
  where
    simScore i j = scoreTable!!i!!j
    scoreTable = [[ score i j | j<-[0..]] | i<-[0..] ]

    score :: Int -> Int -> Int
    score 0 0 = 0
    score 0 j = scoreSpace + simScore 0 (j-1)
    score i 0 = scoreSpace + simScore (i-1) 0
    score i j
      | x == y    = scoreMatch + simScore (i-1) (j-1)
      | otherwise = maximum [scoreSpace + simScore i (j-1),
                            scoreSpace + simScore (i-1) j,
                            scoreMismatch + simScore (i-1) (j-1)]
      where
         x = xs!!(i-1)
         y = ys!!(j-1)


optAlignments2 :: String -> String -> [AlignmentType]
optAlignments2 xs ys = map (reverse *** reverse) $ snd $ opt (length xs) (length ys)
  where
    opt i j = alignTable!!i!!j
    alignTable = [[ optAlign i j | j<-[0..]] | i<-[0..] ]

    optAlign :: Int -> Int -> (Int, [AlignmentType])
    optAlign 0 0 = (0, [("", "")])
    optAlign 0 j = let
      (score, alignment) = opt 0 (j-1)
      in (score + scoreSpace, attachHeads '-' y alignment) --scoreSpace + simScore 0 (j-1)
      where
        y = ys!!(j-1)
    optAlign i 0 = let
      (score, alignment) = opt (i-1) 0
      in (score + scoreSpace, attachHeads x '-' alignment)--coreSpace + simScore (i-1) 0
      where
        x = xs!!(i-1)
    optAlign i j = let
      (score1, alignment1) = opt i (j-1)
      (score2, alignment2) = opt (i-1) j
      (score3, alignment3) = opt (i-1) (j-1)
      x = xs!!(i - 1)
      y = ys!!(j - 1)
      maxima = maximaBy fst [(score1 + scoreSpace, attachHeads '-' y alignment1),
                    (score2 + scoreSpace, attachHeads x '-' alignment2),
                    (score3 + if x == y then scoreMatch else scoreMismatch, attachHeads x y alignment3)]
      in (fst (head maxima), concatMap snd maxima)
