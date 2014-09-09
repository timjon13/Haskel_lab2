module BlackJack where
import Cards
import Wrapper
import Test.QuickCheck

aCard1::Card
aCard1 = Card Spades Ace

aCard2::Card
aCard2 = Card Spades King

empty::Hand
empty=Empty

valueRank::Rank->Integer
valueRank (Numeric i) = i
valueRank Ace         = 11 --ToDo || check if its one
valueRank King        = 10
valueRank Queen       = 10
valueRank Jack        = 10

valueCard::Card->Integer
valueCard (Card r c) = valueRank v

numberOfAces::Hand->Integer
numberOfAces Empty = 0
numberOfAces
numberOfAces

value::Hand->Interger
value hand| 
	  | otherwise valueofhand

gameOver::Hand->Bool
gameOver hand= valueofhand > 21

winner::Hand->Hand->Player
winner handOfGuest handOfBank | gameover(handOfGuest) = Bank 
                              | gameover(handOfBank) = Guest
                              | handOfGuest < handOfBank = Bank
                              | handOfGuest > handOfBank = Guest
                              | handOfGuest == handOfBank= Bank

