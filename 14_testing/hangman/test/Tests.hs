module Tests where

import Hangman 
import Test.Hspec 
import Test.QuickCheck
import Data.Char



-- fillInCharacter takes a Puzzle and a character, and if the 
-- character is used within the Puzzle word, returns the 
-- Puzzle with the updated Puzzle.

-- To test the function, we look at the following properties
--  1) Updating the "guessed" list
--  2) Updating the "discovered" list 
--
-- The methodology for testing the properties are as follows:
--   - Generate a random word from the wordlist and use it for
--     the test
--   - Test "guessed" updating by generating wrong character
--     and comparing output
--   - Test "discovered" updating by generating correct 
--     characters and comparing output (both the "discovered" 
--     list and the "guessed" list have to be updated)
--
-- An instance of Eq Puzzle had to be written in src/Hangman.hs
-- so that the comparisons can be made.

prop_fillInCharacter_guessed :: Puzzle -> Char -> Property
prop_fillInCharacter_guessed p@(Puzzle pwrd d guessed) c =
  not (elem c pwrd) ==> fillInCharacter p c == (Puzzle pwrd d (c:guessed))

prop_fillInCharacter_discovered :: Puzzle -> Property
prop_fillInCharacter_discovered p@(Puzzle pwrd _ g) =
  forAll discGen (\c -> fillInCharacter p c == (Puzzle pwrd (d' c) (c:g)))
    where d' c'= map (\x -> if x == c' then Just c' else Nothing) pwrd 
          discGen :: Gen Char
          discGen = elements pwrd


-- handleGuess takes a Puzzle and a character, and processes the inputs
-- in the following ways.
--
--    1) Character has already been guessed
--        
--       handleGuess prints a line informing the presence of character
--       in the "guessed" list alr, and returns the original Puzzle data
--       structure
--
--    2) Character has not been guessed, but is not in the puzzleword
--
--      handleGuess prints a line informing that it is a wrong guess,
--      appends the input character into the "guessed" list, and returns
--      a new Puzzle datastructure with the updated "guessed" list.
--
--    3) Character has not been guessed, and is within the puzzleword
--
--       handleGuess prints a line informing that it is a correct guess,
--       appends the input character into the "guessed list, and also 
--       updates the "discovered" list (which will be rendered to the
--       user). The new Puzzle data structure is returned.
--
--  To test the handleGuess function, we can look at the printing of 
--  appropriate strings, as well as the return value.

prop_handleGuess_wrongGuess :: Puzzle -> Char -> Property 
prop_handleGuess_wrongGuess p@(Puzzle pwrd _ g) c  = 
  undefined

prop_handleGuess_alrGuessed :: Puzzle -> Property
prop_handleGuess_alrGuessed p@(Puzzle pwrd _ g) =
  undefined

prop_handleGuess_correctGuess :: Puzzle -> Property
prop_handleGuess_correctGuess p@(Puzzle pwrd _ g) =
  undefined

main :: IO ()
main = do 
  word <- randomWord'
  let puzzle = freshPuzzle (fmap toLower word)
  hspec $ do
    describe "fillInCharacter" $ do
      it "wrong characters updates guessed correctly" $
        property $ prop_fillInCharacter_guessed puzzle
      it "correct characters update guessed & discovered correctly" $ 
        prop_fillInCharacter_discovered puzzle
