module Node.HTTP.Vary (vary) where

import Prelude

import Effect (Effect)
import Data.Array (nub, (:))
import Data.Maybe (Maybe, maybe)
import Data.String (Pattern(..), split, joinWith)
import Foreign.Object (Object, lookup)
import Node.HTTP (Response, setHeader)



vary :: Response -> String -> Effect Unit
vary res val = setHeader res "Vary" $ maybe val add $ responseHeader res "vary"
  where
    add vary' = joinWith divider $ nub $ val : split pattern vary'



responseHeader :: Response -> String -> Maybe String
responseHeader res key = lookup key $ responseHeaders res



divider :: String
divider = ","



pattern :: Pattern
pattern = Pattern divider



foreign import responseHeaders :: Response -> Object String
