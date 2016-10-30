module Utils where

import Data.Foldable
import Data.Either

type Tuple6 a = (a,a,a,a,a,a)
type Tuple5 a = (a,a,a,a,a)
type Error = String


findWithErrMsg :: (Show (t a), Foldable t) => (a -> Bool) -> t a -> Either Error a
findWithErrMsg pred list = case find pred list of
    (Just v) -> Right v
    Nothing  -> Left $ "In findWithErrMsg: The function 'find' failed for list:" ++ (show list) ++ "."
