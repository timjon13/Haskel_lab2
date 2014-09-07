import test.QuickCheck

--data type for suit of a cards
Data Suit =♠| ♥ |♣ |♦
 deriving (Eq,Show)

--data type for the color of cards
Data Color = Black|Red
 deriving (Eq,Show)


color::Suit -> Color
color ♠ =Black
color ♥ =Red
color ♣=Black
color ♦=Red

--type for rank of a card
Data Rank= Numeric Integer |Jack|Queen|King|Ace
	deriving (Eq,Show)

--Rankbeats rank1 rank2 checks if rank1 beats rank2
rankBeats:: Rank-> Rank->Bool
rankBeats  _ Ace = false
rankBeats Ace _ = true
rankBeats  _ King = false
rankBeats King _ = true
rankBeats  _ Queen = false
rankBeats Queen _ = true
rankBeats  _ Jack = false
rankBeats Jack _ = true
rankBeats (Numeric m) (Numeric n) = m > n

prop_RankBeats a b =
 a /= b ==>
	rankBeats a b || rankBeats b a 
	

