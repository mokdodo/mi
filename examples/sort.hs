{-# LANGUAGE TemplateHaskell,QuasiQuotes #-}
import Language.MI.TH

[mi|
class DEq a where neq::a->a->Bool
                  ndq::a->a->Bool
                  gt:: a->a->Bool|]

[mi|instance deq1::DEq Integer where
       neq a b = (a `mod` 2 == 0) && (b `mod` 2 == 0)
       ndq a b = (a `mod` 2 == 1) && (b `mod` 2 == 1)
       gt  a b = a < b|]

[mi|instance deq2::DEq Integer where
       neq a b = (a `mod` 3 == 0) && (b `mod` 3 == 0)
       ndq a b = (a `mod` 3 == 1) && (b `mod` 3 == 1)
       gt a b = a > b|]

[mi|
mi = deq1 where 
 check [] = []
 check (c:cs) = (c `neq` 1) : (check cs)|]


[mi|
mi = deq1 where 
 qsort []     = []
 qsort (p:xs) = qsort [x | x <- xs, x `gt` p] ++ [p] ++ qsort [x | x <- xs, not $ x `gt` p] 
|]

main = do
    print $ qsort d
    print $ qsort' deq2 d 
  where
    d = [2, 1, 4, 6, 3, 5]
