module Data where
import Object
import Person

data Data = Data {
                    person :: Person
                 } deriving (Show, Eq)
