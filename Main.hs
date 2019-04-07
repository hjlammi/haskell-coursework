module Main where

import System.IO
import System.Environment
import Parser
import Data
import Person
import qualified Data.Map.Strict as Map

main :: IO ()
main = do
  args <- getArgs
  let fileName = head args in do
    contents <- readFile fileName
    let facts = lines contents in do
      let parsedFact = parseLine $ head facts in do
        question <- getLine
        return ()
        -- let answer = answerOne parsedFact question in do
          -- print answer


-- answerOne :: Fact -> String -> String
-- answerOne parsedData question =
--   let parsedQuestion = qParser question in do
--     if parsedQuestion == parsedData
--       then "yes"
--     else
--       "no"

parseLine :: String -> Fact
parseLine line = parse $ line

updateData :: Data -> Fact -> Data
updateData dataElem (PersonMovesFact f)  =
  let name = personName f
      location = personLocation f
      updatedPerson = (Person name (Just location) Nothing)
  in (Data $ Map.insert name updatedPerson $ persons dataElem)
