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
      let parsedData = mainParser $ head linesOfFile in do
        print parsedData


answerOne :: Data -> String -> String
answerOne parsedData question =
  let parsedQuestion = qParser question in do
    if parsedQuestion == parsedData
      then "yes"
    else
      "no"
