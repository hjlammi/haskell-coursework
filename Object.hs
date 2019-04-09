module Object where

data Object = Object {
                      objectName :: String,
                      objectLocation :: Maybe String
                     } deriving (Show, Eq)
