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
    let linesOfFile = lines contents in do
      let parsedData = readLinesToData linesOfFile [] in do
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

readLinesToData :: [String] -> [Data] -> [Data]
readLinesToData [] [] = []
readLinesToData (line:[]) [] = [mainParser $ line]
readLinesToData (line:lines) dataList
  | elem (mainParser $ line) dataList = replace dataList (mainParser $ line)
  | otherwise = (mainParser $ line) : (readLinesToData lines dataList)

replace :: [Data] -> Data -> [Data]
replace [] dataRecord = [dataRecord]
replace (dataElem:datas) dataRecord
  | (name $ person dataRecord) == (name $ person dataElem) = dataRecord : datas
  | otherwise                                              = dataElem : replace datas dataRecord
