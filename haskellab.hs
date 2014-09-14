module BlackJack where

import Cards --Imports Cards file 
import Wrapper --Imports Wrapper file
import Test.QuickCheck --Imports test.quickcheck

--Task A::
--size hand2 = size (Add (Card (Numeric 2) Hearts) (Add (Card Jack Spades) Empty))
-- = 1 + size (Add (Card Jack Spades)Empty)) => 1+1 + Empty = 2
-- the value checks the size of the hand card and in this  there are two cards and then an empty,which the function then calculates the first card and then the second card to see how many cards it have. Which makes it go a card and another card equals two cards. The resualt will be 1 + 1 =2.  

--Task B::

aCard1::Card --Makes a card function
aCard1 = (Card Ace Spades) --The card being created

aCard2::Card --Makes another card function
aCard2 = (Card Ace Clubs)-- e card being createdTh

aHand::Hand --Makes a hand from the two Cards
aHand= (Add aCard1 (Add aCard2 (Empty))) 

empty::Hand -- Makes a Function if hand is empty
empty = Empty 

valueRank::Rank->Integer --Makes rank for each the cards and also gives them a value
valueRank (Numeric i) = i -- value between (2-10)
valueRank Ace         = 11
valueRank King        = 10
valueRank Queen       = 10
valueRank Jack        = 10

valueCard::Card->Integer -- Checks the value of a Card
valueCard (Card rank suit) = valueRank rank --Creates an value for each card by checking it in valueRank

numberOfAces::Hand->Integer -- Counts the number of aces in the Hand
numberOfAces (Empty) =0 --If the number od aces are empty it returns a 0
numberOfAces (Add (Card Ace _) xs) = 1 +numberOfAces xs --if it is an ace its adds one to number ofaces
numberOfAces (Add _ xs)= numberOfAces xs --if  it finds anoter card it just have the same number f aces as before 
                 			

valuehelp::Hand->Integer  --Counts the value for the Card in the hand and in the rest
valuehelp (Add (Card rank suit) xs) = (valueRank rank) +value xs --Checks the value of all te cards in the hand by summerzing all the value for the cards
valuehelp Empty = 0 -- if it is empty it retunrs a zero

value::Hand-> Integer -- Counts the value of the hand  and takes away all the value for aces ,if the sum is over 21.
value hand|valuehelp(hand) > 21 = (valuehelp(hand)-10*numberOfAces(hand))-- if the hand goes over the value of 21 it takes all the number of aces and minus it from the sum   
          |otherwise = (valuehelp(hand)) -- if the vaue dosnt change it stays the same


gameOver::Hand->Bool -- Checks if the hand is still over 21 from the value of the hand
gameOver hand = (value hand) >21 -- If its over 21 its game over

winner::Hand->Hand->Player -- Checks who is the winner by compaering the value from the player to the value of the bank.
winner handOfGuest handOfBank | gameOver handOfGuest = Bank 
                              | gameOver handOfBank = Guest
                              | (value handOfGuest) > (value handOfBank) =Guest
			      | otherwise = Bank
