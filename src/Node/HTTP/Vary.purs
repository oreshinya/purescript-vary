module Node.HTTP.Vary where

import Prelude
import Control.Monad.Eff (Eff)
import Data.Array (nub, (:))
import Data.Maybe (Maybe, maybe)
import Data.String (Pattern(..), split, joinWith)
import Data.StrMap (StrMap, lookup)
import Node.HTTP (HTTP, Response, setHeader)



vary :: forall e. Response -> String -> Eff (http :: HTTP | e) Unit
vary res val = setHeader res "Vary" $ maybe val add $ responseHeader res "vary"
  where
    add vary' = joinWith divider $ nub $ val : split pattern vary'



responseHeader :: Response -> String -> Maybe String
responseHeader res key = lookup key $ responseHeaders res



divider :: String
divider = ","



pattern :: Pattern
pattern = Pattern divider



foreign import responseHeaders :: Response -> StrMap String
