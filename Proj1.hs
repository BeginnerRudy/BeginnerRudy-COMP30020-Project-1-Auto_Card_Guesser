-- module Proj1 (feedback, initialGuess, nextGuess, GameState) where

--　Used Modules
import Card
import Data.List

-- Main function prototypes
-- feedback :: [Card] -> [Card] -> (Int, Int, Int, Int, Int)
initialGuess :: Int -> ([Card], GameState)
-- nextGuess :: ([Card], GameState) -> (Int, Int, Int, Int, Int) -> ([Card], GameState)

data GameState = GameState|Nothing
    deriving Show

-- This function is responsible for generating initial guess
-- depneds on number of card specified by the user.
-- There are no repeated card, and each card are chosen randomly
initialGuess n 
    | n <= 0 = error "Please Enter Card Number Between 1 to 52"
    | otherwise = (initialGuessHelper n full_deck, GameState)
        where full_deck = [Card x y | x <- [Club ..], y <- [R2 ..]]

-- This function is the helper function for for initialGuess
-- The purpose of it is to generate n number of intial pick from the full deck
-- This function tries its best to randomly pick cards.
-- This function gurantees that there is no repeated catd picked.
initialGuessHelper :: Int -> [Card] -> [Card]
initialGuessHelper n deck
    | n == 0 = []
    | otherwise = selected_card : initialGuessHelper (n-1) remaining_deck
        where random_pick = (n * 20) `mod` (length deck)
              selected_card = deck!!random_pick
              remaining_deck = delete selected_card deck
