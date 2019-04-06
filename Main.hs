module Main where

import System.IO
import System.Environment
import Parser
import Data
import Person

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
-- parseLine (line:lines) = (parse $ line) : (parseLine lines)

-- replace :: [Data] -> [Data]
-- replace [] = []
-- replace [dataElem] = [dataElem]
-- replace (dataElem:rest)
--   | (name $ person dataElem) == (name $ person $ head rest) = rest
--   | otherwise = dataElem : replace rest
-- replace (dataElem:datas) dataRecord
--   | (name $ person dataRecord) == (name $ person dataElem) = dataRecord : datas
--   | otherwise                                              = dataElem : replace datas dataRecord
