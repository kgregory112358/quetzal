module Main where

import Prelude hiding (or, concat)
import Network.Curl.Download
import Text.HTML.TagSoup.Tree
import Text.HTML.TagSoup
import Data.Acid
import Numeric.LinearProgramming
import Data.Dates
import Data.Text(toLower, pack, Text, empty)
import Data.Text.Encoding
import Data.Maybe
import Data.Bool
import Control.Monad
import System.IO

import Jeopardy


main :: IO ()
main = do
    result <- openURI "http://www.j-archive.com/showgame.php?game_id=5438"
    case result of
        (Right value) -> do
            putStrLn $ show $ extractJeopardyGame $ parseTree $ decodeUtf8 value
        (Left error) -> putStrLn $ show error
    return ()
