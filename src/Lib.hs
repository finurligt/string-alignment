module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

similarityScore :: String -> String -> Int
similarityScore (x:xs) [] = similarityScore xs [] + scoreSpace
similarityScore [] (y:ys) =  similarityScore ys [] + scoreSpace
similarityScore [] [] = 0
similarityScore (x:xs) (y:ys) = maximum [similarityScore xs ys + if x == y then scoreMatch else scoreMismatch,
                                      similarityScore xs y:ys + scoreSpace,
                                      similarityScore x:xs ys + scoreSpace ]

maximaBy :: Ord b => (a -> b) -> [a] -> [a]
maximaBy :: f xs = foldl () [] xs
