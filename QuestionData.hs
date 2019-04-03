module QuestionData where

import Object
import Person

data QuestionData = QuestionData {
                    p :: Person
                 } deriving (Show, Eq)
