-- COMP30020 Declarative Programming
-- Author   : Tao Wang
-- Purpose  : The implementation of "The Game of ChordProbe"

-- | This code is the submission for the project1 "The Game of ChordProbe" 
--   of the subject COMP30020.

-- | Game rules description (From Project1 Specification)
--   The Game of ChordProbe is a two-player logical guessing game, one will 
--   be the composer and the other is the performer. The composer begins by 
---  selecting a three-pitch musical chord, where each pitch comprises a 
--   musical note, one of A, B, C, D, E, F, or G, and an octave, 
--   one of 1, 2, or 3. This chord will be the target for the game. 
--   The order of pitches in the target is irrelevant, and no pitch may 
--   appear more than once. This game does not include sharps or flats, 
--   and no more or less than three notes may be included in the target.

-- | How to play the game (From Project1 Specification)
--   Once the composer has selected the target chord, the performer 
--   repeatedly chooses a similarly defined chord as a guess and tells 
--   it to the composer, who responds by giving the performer the following 
--   feedback: correct pitches, correct notes, and correct octaves. 
  
-- | NOTE: In counting correct notes and octaves, multiple occurrences in the
--   guess are only counted as correct if they also appear repeatedly 
--   in the target. Correct pitches are not also counted as correct 
--   notes and octaves.


module Proj1 (initialGuess,nextGuess,GameState) where

import Data.List
import Data.Ord
import qualified Data.Set as Set

-- | Define the GameState Type 
--   The game state is a list of remaining candidate guesses.
--   The "initialGuess" initialises the game state while the "nextGuess"
--   updates it.

data GameState = GameState {
    searchSpace :: [[String]]
} deriving(Show)

-- | Define the global constants
--   The global constants in this game include: the number of guessing pitches,
--   the range of notes, and the range of octaves, and the space of all 
--   possible combined pitches. Also in order to maximize the feedback 
--   received after the initialGuess, hence the initGuess is defined as 
--   3 different pitches with different notes and octaves. 

numPitches = 3
notesSpace = ['A'..'G']
octavesSpace = ['1'..'3']
pitchesSpace = sequence [notesSpace,octavesSpace]
initGuess = ["A1","B2","C3"]



-- ******************************************************
-- *                                                    *
-- *     "initialGuess" and Related Helper Functions    *
-- *                                                    *
-- ******************************************************


-- | InitialGuess Function
--   Takes no input arguments, and returns a pair of an initial guess and 
--   a game state.
-- 
--   The following helper function(s) are used: 
--   1. "getInitSearchSpace": to enumerate all possible valid candidate guesses 
--      at the start.
--   2. "uniqueList": to check if all pitches of a candidate are unique.
--   3. "sort" : to sort the pitches of a candidate guess.
--   4. "nud" : to remove duplicated candidate guesses.

initialGuess :: ([String],GameState)
initialGuess = (initGuess,gameState)
  where gameState = GameState {searchSpace = getInitSearchSpace numPitches}


-- | Helper function : "getInitSearchSpace"
--   Purpose: To enumerate all possible valid candidate guesses at the start.
--   1. "uniqueList" helper function here works for removing the candidates that 
--   contain some same pitches.
--   2. "sort" function here works for sorting the pitches of candidate guesses, 
--   so that we are able to identify and remove (by nud) those duplicated 
--   candidates that have the same pitches but in different orders originally.

getInitSearchSpace :: Int -> [[String]]
getInitSearchSpace numPitches = nub[ sort x | x <- combinations, uniqueList x ]
  where combinations = sequence $ replicate numPitches pitchesSpace

-- | Helper function: "uniqueList"
--   Purpose: To detect if all the elements are unique in a list
uniqueList :: (Eq a) => [a] -> Bool
uniqueList [] = True
uniqueList (x:xs) = x `notElem` xs && uniqueList xs



-- ******************************************************
-- *                                                    *
-- *     "nextGuess" and Related Helper Functions       *
-- *                                                    *
-- ******************************************************

-- | nextGuess Function 
--   Inputs: a pair of the previous guess and game state, and the feedback to 
--   this guess as a triple of correct pitches, notes, and octaves. 
--   Return: returns a pair of the next guess and game state.
-- 
--   The following helper functions are used:
--   1. "reduceSearchSpaceSize": to reduce the search space size based on the
--       previous guesses and feedback (Hint 2)
--   2. "updateGameState" : to update the GameState with the reduced search space
--   3. "getOptimalGuess" : to get the best possible next guess (Hints 3,5)

nextGuess :: ([String],GameState) -> (Int,Int,Int) -> ([String],GameState)
nextGuess (previousGuess,gameState) feedback = (newGuess,newGameState)
  where noPreSearchState = searchSpace gameState \\ [previousGuess]
        reducedSpace = reduceSearchSpaceSize 
                       (previousGuess,updatedGameState) feedback
          where updatedGameState = updateGameState gameState noPreSearchState
        newGameState = updateGameState gameState reducedSpace 
        newGuess = getOptimalGuess newGameState


-- | Helper function: "reduceSearchSpaceSize" (Hint 2)
--   Inputs: previous guess, GameState, feedback to previous guess
--   Return: a reduced search space, namely, a list of all remaining possible answers
--   
--   Idea behind: The search space size can be largely reduced by computing
--   the list of possible targets, and removing elements that are inconsistent 
--   with any answers you have received to previous guesses. 

--   The following helper function is used:
--   1. "response": to compute the correct answer to a guess.

reduceSearchSpaceSize :: ([String],GameState) -> (Int,Int,Int) -> [[String]]
reduceSearchSpaceSize (previousGuess,gameState) feedback = newSearchSpace
  where newSearchSpace = 
          [candidate | candidate <- searchSpace gameState, 
                       response previousGuess candidate == feedback]

-- | Helper function: "updateGameState" 
--  Update GameState by replacing the previous search space with 
--  newly reduced search space.

updateGameState :: GameState -> [[String]] -> GameState
updateGameState gameState updatedSpace = gameState {searchSpace = updatedSpace}


-- | Helper function: "getOptimalGuess"
--  Input: GameState
--  Return: The best possible next guess
--
--  Idea behind: Compute the average number of possible targets that will remain 
--  after each guess, giving the expected number of remaining possible targets 
--  for each guess, and choose the guess with the smallest expected number of 
--  remaining possible targets. (Hint 3,5)
--  
--  The following helper functions are used:
--  1."getFeedbackWeight": to compute the weight/frequency for each kind of feedback
--  2."getAverageRemainingGuessNum": compute the expectation for each candidate guesses
--  3."getAverageExpectionsList": to get a list of expectation of all candidate guesses
--  4."minIndex": to get the index of the minim element in a list  

getOptimalGuess :: GameState -> [String]
getOptimalGuess gameState = (searchSpace gameState) !! index
  where guesses = getAverageExpectationsList gameState 
        index = minIndex (guesses)


-- ********** Helper Functions used for "nextGuess" ************** --

-- | Helper function : "response" (from Proj1Test.hs line 16-24)
--   Purpose : Compute the correct answer to a guess.  
--   First argument is the target, second is the guess.
response :: [String] -> [String] -> (Int,Int,Int)
response target guess = (right, rightNote, rightOctave)
  where guess'      = nub guess
        right       = length $ intersect guess' target
        num         = length guess'
        rightNote   = num - (length $ deleteFirstsBy (eqNth 0) guess' target) 
                    - right
        rightOctave = num - (length $ deleteFirstsBy (eqNth 1) guess' target) 
                    - right

-- | Helper function: "eqNth" (from Proj1Test.hs line 29-30)
--   eqNth n l1 l2 returns True iff element n of l1 is equal to 
--   element n of l2.
eqNth :: Eq a => Int -> [a] -> [a] -> Bool
eqNth n l1 l2 = (l1 !! n) == (l2 !! n)

-- | Helper function: "minIndex"
--  Purpose: to get the index of the minum element in a list 
minIndex ::  Ord a => [a] -> Int
minIndex = fst . minimumBy (comparing snd) . zip [0..]

-- | Helper function: "getFeedbackWeight": 
--  Purpose: to compute the weight/frequency for each kind of feedback
getFeedbackWeight :: [String] -> GameState -> [Int]
getFeedbackWeight guess gameState = map length (group feedbackWeightLists)
   where  feedbackWeightLists = map (flip response guess) 
                                   (searchSpace gameState)

-- | Helper function: "getAverageRemainingGuessNum": 
--  Purpose: to compute the expectation for each candidate guesses
getAverageRemainingGuessNum :: [String] -> GameState -> Double
getAverageRemainingGuessNum guess gameState = 
  (fromIntegral ( sum (map (^2) feedbackWeight)))/
                            (fromIntegral(sum(feedbackWeight)))
   where feedbackWeight = getFeedbackWeight guess gameState

-- | Helper function: "getAverageExpectionsList": 
--  Purpose: to get a list of expectation of all candidate guesses
getAverageExpectationsList :: GameState -> [Double]
getAverageExpectationsList gameState = 
 [getAverageRemainingGuessNum guess gameState | 
                            guess <- (searchSpace gameState)]




          


        
