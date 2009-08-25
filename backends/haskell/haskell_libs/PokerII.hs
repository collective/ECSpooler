module PokerII where
import Data.List

data Rank = Two|Three|Four|Five|Six|Seven|Eight|Nine|Ten|J|Q|K|A
          deriving (Eq, Ord, Enum, Show, Read)

data Suit = Spades|Hearts|Diamonds|Clubs
          deriving (Eq, Show, Read)       -- no ordering for poker

data Card = Card Rank Suit
          deriving (Show, Read)

data Hand = Hd [Card]         
          
instance Eq Card where
    (Card rank1  _)  == (Card rank2 _)  = rank1 == rank2


instance Ord Card where
   compare (Card rank1 _ ) (Card rank2 _)
   	  |  rank1 < rank2 = LT
        |  rank1 > rank2 = GT
        |  otherwise = EQ
                                                              

isFlush [_] = True
isFlush ((Card _ suit1):(Card r suit2):cards)
        | suit1 == suit2 = isFlush ((Card r suit2):cards)
        | otherwise = False


isStraight cards =  xs == [0,1,2,3,12] || inSequence xs 
	   where xs = (sort [fromEnum rank | (Card rank _) <- cards])
inSequence [_] = True
inSequence (x:y:xs)
	| (y-x) == 1 = inSequence (y:xs)
	| otherwise = False

isStraightFlush cards = isStraight cards && isFlush cards

isRoyalFlush cards = isFlush cards && (sort [rank | (Card rank _) <- cards]) == [Ten .. A]