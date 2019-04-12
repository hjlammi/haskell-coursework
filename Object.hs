module Object where
import qualified Data.Map.Strict as Map

data Object =
  Object {
    objectLocation :: ObjectLocation
  } deriving (Show, Eq)

data ObjectLocation =
  ObjectLocation {
    onPerson :: Maybe String,
    inLocation :: Maybe String
  } deriving (Show, Eq)
