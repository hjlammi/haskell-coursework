module Object where

data Object = Object {
                      objectName :: String
                     } deriving (Show, Eq)

getName :: Object -> String
getName (Object { objectName = n }) = n
