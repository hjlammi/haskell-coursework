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
  PersonLocationQuestion PersonLocationQ |
  PersonLocationBeforeQuestion PersonLocationBeforeQ |
  PersonLocationAfterQuestion PersonLocationAfterQ |
  ObjectLocationQuestion ObjectLocationQ |
  NumOfObjectsQuestion NumOfObjectsQ
  deriving (Show, Eq)

data PersonLocationQ =
  PersonLocationQ {
    personLocationName :: String,
    personLocationLocation :: String
  } deriving (Show, Eq)

data PersonLocationBeforeQ =
  PersonLocationBeforeQ {
    personLocationBeforeName :: String,
    personLocationBeforeLocation :: String
  } deriving (Show, Eq)

data PersonLocationAfterQ =
  PersonLocationAfterQ {
    personLocationAfterName :: String,
    personLocationAfterLocation :: String
  } deriving (Show, Eq)

data ObjectLocationQ =
  ObjectLocationQ {
    objectName :: String
  } deriving (Show, Eq)

data NumOfObjectsQ =
 NumOfObjectsQ {
   ownerName :: String
 } deriving (Show, Eq)
