module Data where
import Object
import Person
import qualified Data.Map.Strict as Map

data Data =
  Data {
    persons :: Map.Map String Person,
    objects :: Map.Map String Object
  } deriving (Show, Eq)

data Question =
  PersonQuestion PQ |
  ObjectQuestion OQ |
  NumOfObjectsQuestion NumQ
  deriving (Show, Eq)

data PQ =
  PQ {
    subject :: String,
    place :: String
  } deriving (Show, Eq)

data OQ =
  OQ {
    objectName :: String
    -- objectPlace :: Maybe String
  } deriving (Show, Eq)

data NumQ =
 NumQ {
   ownerName :: String
 } deriving (Show, Eq)
