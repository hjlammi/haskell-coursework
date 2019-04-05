module Main where

import System.IO
import System.Environment
import Parser
import Data

main :: IO ()
main = do
  args <- getArgs
  let fileName = head args in do
    contents <- readFile fileName
    let linesOfFile = lines contents in do
      let parsedData = readLinesToData linesOfFile in do
        question <- getLine
        let answer = answerOne (head parsedData) question in do
          print answer


answerOne :: Data -> String -> String
answerOne parsedData question =
  let parsedQuestion = qParser question in do
    if parsedQuestion == parsedData
      then "yes"
    else
      "no"

readLinesToData :: [String] -> [Data]
readLinesToData [] = []
readLinesToData (line:[]) = [mainParser $ line]
readLinesToData (line:lines) = (mainParser $ line) : (readLinesToData lines)
