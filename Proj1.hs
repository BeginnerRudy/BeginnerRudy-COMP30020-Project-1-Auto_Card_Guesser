{-*********************************************************
  *            COMP30020 Declarative Programming          *
  *             - Project 1 (Auto Card Guesser)           *
  *                Author Name: Renjie Meng               *
  *                    Date: 2019/08/27                   *
  *                        ~ 2019/                        *
  *********************************************************-}

module Proj1 (feedback, initialGuess, nextGuess, GameState) where

--　Used Modules
import Card
import Data.List
import Data.Ord

data GameState = GuessSapce [[Card]] Int 
    deriving Show

-- This function is responsible for generating initial guess
-- depneds on number of card specified by the user. There are no repeated card
-- This function only handle intial guess up to 4 cards
initialGuess :: Int -> ([Card], GameState)
initialGuess n 
    | n <= 0 = error "Please Enter Card Number Between 1 to 52"
    | otherwise = (guess, GuessSapce full_guess_space 1)
        where suits = take n [Club ..]
              ranks = take n (every (13 `div` n) [R2 ..])
              guess = zipWith Card suits ranks
              full_deck_1_dim = [[Card suit rank] | suit <- [Club ..], rank <- [R2 ..]]
              full_guess_space = generateFullGuessSapce n full_deck_1_dim

-- Assume that the Int > 0
-- This function is responsible for generating all the possible guesses of n cards  
-- The name hyperPlane refers to the guessGuess which is 1-dim less than the one going to construct            
generateFullGuessSapce :: Int -> [[Card]] -> [[Card]]
generateFullGuessSapce dim_required hyperPlane
    | dim_required == 1 = hyperPlane -- stop when the dimension is equal to 1, since the input is 1-dim. no more to add
    | otherwise = generateFullGuessSapce (dim_required-1) [x++y | x<-full_deck_1_dim, y<-hyperPlane, (elem (x!!0) y) == False ]
        where
            full_deck_1_dim = [[Card suit rank] | suit <- [Club ..], rank <- [R2 ..]]

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
feedback :: [Card] -> [Card] -> (Int, Int, Int, Int, Int)
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

-- *****************Helper functions for the feedback function Start***************
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
-- *****************Helper functions for the feedback function End*****************

-- nextGuess :: ([Card], GameState) -> (Int, Int, Int, Int, Int) -> ([Card], GameState)
nextGuess (last_guess, GuessSapce last_guess_space count) last_feedback = (next_guess, next_GameState)
    where
        reduced_guess_space = removeInconsistent last_guess last_guess_space last_feedback
        next_guess = pickBestGuess reduced_guess_space count
        next_GameState = GuessSapce (delete next_guess reduced_guess_space) (count+1)

-- eleminate inconsistent guess
removeInconsistent :: [Card] -> [[Card]] -> (Int, Int, Int, Int, Int) -> [[Card]]
removeInconsistent _  [] _ = []
removeInconsistent last_guess  (x:xs) last_feedback
    | feedback x last_guess == last_feedback = x : removeInconsistent last_guess xs last_feedback
    | otherwise = removeInconsistent last_guess xs last_feedback

-- pick best guess candidate
pickBestGuess :: [[Card]] -> Int -> [Card]
pickBestGuess [] _ = []
pickBestGuess possibleAnswer count 
    | count <= 2 = head possibleAnswer
    | otherwise = getGuess (minimumBy (comparing snd) allExpectedGuessSpaceSize)
    where allExpectedGuessSpaceSize = [(x, generateGuessSapceSize x possibleAnswer)| x <- possibleAnswer]

-- get [Card] from a tuple ([Card], Double)
getGuess :: ([Card], Double) -> [Card]
getGuess (guess, _) = guess

-- find all feedback
-- Assume the guess given is not empty and it has same length with each possible answer in guess space
feedbackAll :: [Card] -> [[Card]] -> [(Int, Int, Int, Int, Int)]
feedbackAll _ [] = []
feedbackAll guess (possibleAnswer:remainGuessSpace) = feedback possibleAnswer guess : feedbackAll guess remainGuessSpace


-- generate a expectedGuessSpaceSize
generateGuessSapceSize :: [Card] -> [[Card]] -> Double
generateGuessSapceSize _ [] = 0
generateGuessSapceSize guess possibleAnswer =  expectedSize
    where allPossibleFeedback = feedbackAll guess possibleAnswer
          guessSpaceSizeDistribution = map length (group allPossibleFeedback)
          expectedSize =  (fromIntegral (sum (map (^2) guessSpaceSizeDistribution))) / (fromIntegral (sum guessSpaceSizeDistribution))