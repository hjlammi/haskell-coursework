module Data where
import Object
import Person

data Data = Data {
                    persons :: [Person]
                 } deriving (Show, Eq)

data Fact =
  PersonMovesFact PersonMoves deriving (Show, Eq)
