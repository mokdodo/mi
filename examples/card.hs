{-# LANGUAGE TemplateHaskell,QuasiQuotes #-}
import Language.MI.TH

data Card = Card Suit Value deriving (Show)
data Suit = Spade | Heart | Diamond | Club deriving (Show,Eq)
type Value = Int

[mi|
class CardEq a where
  ceq::a->a->Bool
|]

[mi|
instance meq::CardEq Card where
  ceq (Card m1 v1) (Card m2 v2) = m1 == m2
|]

[mi|
instance veq::CardEq Card where
  ceq (Card m1 v1) (Card m2 v2) = v1 == v2
|]

[mi|
instance alleq::CardEq Card where
  ceq (Card m1 v1) (Card m2 v2) = (m1 == m2) && (v1 == v2)
|]

[mi|
mi = alleq where
  select a b = filter (\x -> x `ceq` b) a|]

allCard = [Card m v | v <- [1..13], m <- [Spade, Heart, Diamond, Club]]

main = do
    print $ select allCard card
    print $ select' meq allCard card
    print $ select' veq allCard card
  where
    card = Card Spade 1
