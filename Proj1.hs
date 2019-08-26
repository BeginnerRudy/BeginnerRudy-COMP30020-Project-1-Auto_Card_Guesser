module Proj1 (feedback, initialGuess, nextGuess, GameState) where

--　Used Modules
import Card

-- Main function prototypes
feedback :: [Card] -> [Card] -> (Int, Int, Int, Int, Int)
initialGuess :: Int -> ([Card], GameState)
nextGuess :: ([Card], GameState) -> (Int, Int, Int, Int, Int) -> ([Card], GameState)

data GameState = GameState|Nothing