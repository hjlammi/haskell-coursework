module Data where
import Object
import Person
import qualified Data.Map.Strict as Map

data Data =
  Data {
    persons :: Map.Map String Person,
    objects :: Map.Map String Object
  } deriving (Show, Eq)
