-- module Proj1 (feedback, initialGuess, nextGuess, GameState) where

--　Used Modules
import Card
import Data.List

-- Main function prototypes
feedback :: [Card] -> [Card] -> (Int, Int, Int, Int, Int)
initialGuess :: Int -> ([Card], GameState)
-- nextGuess :: ([Card], GameState) -> (Int, Int, Int, Int, Int) -> ([Card], GameState)

data GameState = GameState|FirstGuess
    deriving Show

-- This function is responsible for generating initial guess
-- depneds on number of card specified by the user. There are no repeated card
-- This function only handle intial guess up to 4 cards
initialGuess n 
    | n <= 0 = error "Please Enter Card Number Between 1 to 52"
    | otherwise = (zipWith Card suits ranks, FirstGuess)
        where suits = take n [Club ..]
              ranks = take n (every (13 `div` n) [R2 ..])


-- This function takes every nth element from a list and return them as a new list   
every :: Int -> [a] -> [a]
every n list 
    | (n-1) > length list = []
    | otherwise = x : every n xs
        where (x:xs) = drop (n-1) list

-- This function is responsible for giving feedback for the player's guess
-- Return:
--          num_correct_card  -> The number of cards player guessed correctly
--          num_lower_rank    -> The number of cards in the answer which has 
--                                  lower rank than the lowest rank in the guess
--          num_correct_rank  -> The number of cards in the answer has same
--                                  rank in the guess
--          num_higher_rank   -> The number of cards in the answer which has 
--                              　  higher rank than the highest rank in the guess
--          num_correct_suit  -> The number of cards in the answer has same
--                                  suit in the guess
-- For more information, please read the project specification
feedback target guess 
    | (length target) /= (length guess) = error "Guess and Target do not have the same length"
    | otherwise = (num_correct_card, num_lower_rank, 
                            num_correct_rank, num_higher_rank, num_correct_suit)
    where lowest_rank_guess = getExtremeRank minimum guess
          highest_rank_guess =  getExtremeRank maximum guess
          num_correct_card = length (target `intersect` guess)
          num_lower_rank = length (filter (<lowest_rank_guess) (map getRank target))
          num_correct_rank = numElementsInBothList (map getRank target) (map getRank guess)
          num_higher_rank = length (filter (>highest_rank_guess) (map getRank target))
          num_correct_suit = numElementsInBothList (map getSuit target) (map getSuit guess)

-- *****************Helper functions for the feedback function*****************
-- extract rank
getRank :: Card -> Rank
getRank (Card suit rank) = rank
-- extract suit
getSuit :: Card -> Suit
getSuit (Card suit rank) = suit
-- get lower/ highest rank
getExtremeRank :: ([Rank] -> Rank) -> [Card] -> Rank
getExtremeRank _ [] = error "Empty card deck has no extreme rank"
getExtremeRank f cards = f (map getRank cards)
-- how many elements of lists 1 is in list 2
-- Assume that both list has same length
numElementsInBothList :: Eq a => [a] -> [a] -> Int
numElementsInBothList [] _ = 0
numElementsInBothList _ [] = 0
numElementsInBothList (x:target) guess 
    | elem x guess = 1 + numElementsInBothList target (delete x guess)
    | otherwise = numElementsInBothList target guess

-- nextGuess (last_guess, last_game_state) (num_correct_card, num_lower_rank, num_correct_rank, num_higher_rank, num_correct_suit)
