scoreMatch = 0
scoreMismatch = -1
scoreSpace = -1
string1 = "writers"
string2 = "vintner"
x = "aferociousmonadatemyhamster"
y = "functionalprogrammingrules"

score :: Char -> Char -> Int
score x '-' = scoreSpace
score '-' y = scoreSpace
score x y
  | x == y    = scoreMatch
  | otherwise = scoreMismatch

similarityScore :: String -> String -> Int
similarityScore xs [] = scoreSpace * length xs
similarityScore [] ys = scoreSpace * length ys
similarityScore (x:xs) (y:ys) = maximum [ similarityScore xs ys + score x y,
                                          similarityScore xs (y:ys) + score x '-',
                                          similarityScore (x:xs) ys + score '-' y ]

attachHeads :: a -> a -> [([a],[a])] -> [([a],[a])]
attachHeads h1 h2 list = [(h1:xs, h2:ys) | (xs,ys) <- list]

maximaBy :: Ord b => (a -> b) -> [a] -> [a]
maximaBy f xs = [x | x <- xs, f x == maximum (map f xs)]

type AlignmentType = (String,String)

optAlignments :: String -> String -> [AlignmentType]
optAlignments [] [] = [([],[])]
optAlignments (x:xs) [] = attachHeads x '-' (optAlignments xs [])
optAlignments [] (y:ys) = attachHeads '-' y (optAlignments [] ys)
optAlignments (x:xs) (y:ys) = maximaBy (uncurry similarityScore) l
  where l = concat [ attachHeads x y (optAlignments xs ys),
                     attachHeads x '-' (optAlignments xs (y:ys)),
                     attachHeads '-' y (optAlignments (x:xs) ys) ]

optAlignments2 :: String -> String -> [AlignmentType]
optAlignments2 xs ys = map (\(a, b) -> (reverse a, reverse b)) (snd (opt (length xs) (length ys)))
    where
        opt :: Int -> Int -> (Int, [AlignmentType])
        opt i j = optTable !! i !! j
        optTable = [[ optEntry i j | j <- [0..] ] | i <- [0..] ]

        optEntry :: Int -> Int -> (Int, [AlignmentType])
        optEntry 0 0 = (0, [([],[])])
        optEntry i 0 = (scoreSpace * i, [(take i xs, replicate i '-')])
        optEntry 0 j = (scoreSpace * j, [(replicate j '-', take j ys)])
        optEntry i j = (fst (head z), concatMap snd z)
            where
              (a, opta) = opt (i - 1) (j - 1)
              (b, optb) = opt (i - 1) j
              (c, optc) = opt i (j - 1)
              x = xs !! (i - 1)
              y = ys !! (j - 1)
              z = maximaBy fst $ [ (a + score x y, attachHeads x y opta),
                                   (b + score x '-', attachHeads x '-' optb),
                                   (c + score '-' y, attachHeads '-' y optc) ]

outputOptAlignments :: String -> String -> IO ()
outputOptAlignments x y = do
  let a = optAlignments2 x y
  putStrLn ("There are " ++ show (length a) ++ " optimal alignments")
  mapM_ (putStrLn . f) a
    where
      f (a, b) = "\n" ++ g a ++ "\n" ++ g b
      g = unwords . map return
