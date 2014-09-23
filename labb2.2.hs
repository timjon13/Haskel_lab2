module BlackJack where

import Cards --Imports Cards file 
import Wrapper --Imports Wrapper file
import Test.QuickCheck --Imports test.quickcheck
import System.Random-- imports the systeam random function


--Task A::
--size hand2 = size (Add (Card (Numeric 2) Hearts) (Add (Card Jack Spades) Empty))
-- = 1 + size (Add (Card Jack Spades)Empty)) => 1+1 + size Empty = 2
-- the value checks the size of the hand card and in this  there are two cards and then an empty,which the function then calculates the first card and then the second card to see how many cards it have. Which makes it go a card and another card equals two cards. The resualt will be 1 + 1 +size Empty(0) =2.  

--Task B::

aCard1::Card --Makes a card function
aCard1 = (Card Ace Spades) --The card being created

aCard2::Card --Makes another card function
aCard2 = (Card Ace Clubs)-- The card being created

aHand::Hand --Makes a hand from the two Cards
aHand= (Add aCard1 (Add aCard2 (Empty))) 

second::(Hand,Hand) -> Hand
second (_,x) = x

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


(<+):: Hand->Hand->Hand -- given two hands adds one hand to the other  
(<+) Empty aHand2 = aHand2
(<+) (Add card aHand) aHand2 = (Add card (aHand<+aHand2))

prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool 
prop_onTopOf_assoc p1 p2 p3 = p1 <+ (p2 <+ p3) == (p1 <+ p2) <+ p3

prop_size_onTopOf:: Hand->Hand->Bool -- compares the two sizes of the hands and returns a bool
prop_size_onTopOf p1 p2 =(size p1)+(size p2) == size (p1<+p2)

allTheSuits:: Suit->Hand -- all the suits in the cards 
allTheSuits r = (Add(Card Ace r )(Add(Card King r )(Add(Card Queen r )(Add (Card Jack r)(Add (Card(Numeric 10)r)(Add(Card(Numeric 9)r)(Add(Card(Numeric 8)r)(Add(Card(Numeric 7) r)(Add(Card(Numeric 6) r)(Add(Card(Numeric 5) r)(Add(Card(Numeric 4)r)(Add(Card(Numeric 3)r)(Add(Card(Numeric 2) r) Empty)))))))))))))

fullDeck::Hand -- all the colors for the in a deck of cards
fullDeck =(allTheSuits Spades)<+(allTheSuits Hearts)<+(allTheSuits Clubs)<+ (allTheSuits Diamonds)

draw :: Hand-> Hand-> (Hand,Hand) -- draws a card from the deck and places it in the hand 
draw Empty _ =error("draw: The deck is empty")
draw (Add card ahand) ahand2 = (ahand, (Add card ahand2))

playBank:: Hand-> Hand -- the function which starts out with a  deck and then returns the bank final hand
playBank deck = playBank' deck Empty

playBank' :: Hand-> Hand-> Hand --the bankhand helper function which takes the two hands of the bank and returns the final value for the bank hand
playBank' deck  bankHand  
   | value bankHand < 15   = playBank' deck'  bankHand'                     
   | otherwise             = bankHand
 where (deck', bankHand')  = draw deck bankHand

pickLeCard ::Hand-> Integer-> Card --picks a card randomly from the deck
pickLeCard Empty _ = error "Picking from an empty hand. " 
pickLeCard (Add card hand) v | v == 1    = card
		             | otherwise = pickLeCard hand (v-1)

removeLeCard::Hand-> Integer-> Hand --Removes a card from the deck which is random 
removeLeCard (Add card aHand) n| n == 1    = aHand 
                               | otherwise = (Add card (removeLeCard aHand (n-1)))                  

shuffle::StdGen->Hand->Hand   --The main function which does the shuffle function,which calls on the helper function and place the new card in the new deck. 
shuffle g deck = second (shuffle' g deck Empty)  

shuffle'::StdGen ->Hand->Hand-> (Hand,Hand) -- the helper function which picks a card from the  deck and plces it into another deck 
shuffle' g  deck deck2 | deck  == Empty = (Empty,deck2)
		       | otherwise      = (shuffle' g' (removeLeCard deck i) (Add (pickLeCard deck i) deck2))
 where (i,g') = randomR(1,size deck)

	
prop_shuffle_sameCards :: StdGen -> Card -> Hand -> Bool -- a function which checks if the same card have been shuffled
prop_shuffle_sameCards g c h =
   c `belongsTo` h == c `belongsTo` shuffle g h

belongsTo :: Card -> Hand -> Bool -- a helperfunction which checks if the card is in the hand
c `belongsTo` Empty    = False
c `belongsTo` Add c' h = c == c' || c `belongsTo` h

