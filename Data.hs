module Data where
import Object
import Person
import qualified Data.Map.Strict as Map

-- Data structure for the data read from the file:
-- (Routes to rooms not implemented, i.e. task 6)
data Data =
  Data {
    persons :: Map.Map String Person,
    objects :: Map.Map String Object
  } deriving (Show, Eq)
