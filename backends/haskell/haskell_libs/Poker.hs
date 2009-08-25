module Poker where
data Rank = Two|Three|Four|Five|Six|Seven|Eight|Nine|Ten|J|Q|K|A
          deriving (Eq, Ord, Enum, Show, Read)

data Suit = Spades|Hearts|Diamonds|Clubs
          deriving (Eq, Show, Read)       -- no ordering for poker

data Card = Card Rank Suit
          deriving (Show, Read)
          
instance Eq Card where
  (Card rank1 _) == (Card rank2 _) = rank1 == rank2

instance Ord Card where
  compare card1@(Card rank1 suit1) card2@(Card rank2 suit2)
       | card1 == card2 = EQ
       | rank1 < rank2  = LT
       | otherwise      = GT 
       
          