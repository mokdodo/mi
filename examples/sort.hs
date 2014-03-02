{-# LANGUAGE TemplateHaskell,QuasiQuotes #-}
import Language.MI.TH

[mi|
class DEq a where neq::a -> a -> Bool
                  ndq::a -> a -> Bool
                  gt:: a -> a -> Bool|]

[mi|instance deq1::DEq Int where
       neq a b = (a `mod` 2) == (b `mod` 2)
       ndq a b = not $ (neq' a b) -- bug
       gt  a b = a < b|]

[mi|instance deq2::DEq Int where
       neq a b = (a `mod` 3) == (b `mod` 3)
       ndq a b = (a `mod` 3) /= (b `mod` 3)
       gt a b = a > b|]

[mi|
mi = deq1 where 
 check [] = []
 check (c:cs) = (c `neq` 1) : (check cs)|]


[mi|
mi = deq1 where
  qsort :: [Int] -> [Int]
  qsort []     = []
  qsort (p:xs) = qsort [x | x <- xs, x `gt` p] ++ [p] ++ qsort [x | x <- xs, not $ x `gt` p] 
|]

main = do
    print $ qsort d
    print $ qsort' deq2 d 
    print $ check $ qsort d
    print $ check' deq2 $ qsort d
  where
    d = [2, 1, 4, 6, 3, 5]
