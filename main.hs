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
import Data.Either
import System.IO
import Data.Foldable

type Tuple6 a = (a,a,a,a,a,a)
type Tuple5 a = (a,a,a,a,a)
type Error = String

data JeopardyClue = JeopardyClue Text Text deriving (Show, Eq)

data JeopardyCategory = JeopardyCategory Text (Tuple5 (Maybe JeopardyClue)) deriving (Show, Eq)
data JeopardyRound = JeopardyRound Bool (Tuple6 (Maybe JeopardyCategory)) deriving (Show, Eq)
data JeopardyFinal = JeopardyFinal Text Text Text deriving (Show, Eq)
data JeopardyGame = JeopardyGame JeopardyRound JeopardyRound JeopardyFinal deriving (Show, Eq)

fromString :: String -> Text
fromString = pack

main :: IO ()
main = do
    result <- openURI "http://www.j-archive.com/showgame.php?game_id=5438"
    case result of
        (Right value) -> do
            putStrLn $ show $ extractJeopardyGame $ parseTree $ decodeUtf8 value
        (Left error) -> putStrLn $ show error
    return ()

clueText :: Text
clueText = pack "clue"

categoryText :: Text
categoryText = pack "category"

htmlText :: Text
htmlText = pack "html"

bodyText :: Text
bodyText = pack "body"

divText :: Text
divText = pack "div"

idText :: Text
idText = pack "id"

contentText :: Text
contentText = pack "content"

jeopardyRoundText :: Text
jeopardyRoundText = pack "jeopardy_round"

doubleJeopardyRoundText :: Text
doubleJeopardyRoundText = pack "double_jeopardy_round"

finalJeopardyRoundText :: Text
finalJeopardyRoundText = pack "final_jeopardy_round"

tableText :: Text
tableText = pack "table"

classText :: Text
classText = pack "class"

roundText :: Text
roundText = pack "round"

trText :: Text
trText = pack "tr"

finalRoundText :: Text
finalRoundText = pack "final_round"

tdText :: Text
tdText = pack "td"

extractTableCategories :: TagTree Text -> Either Error [Text]
extractTableCategories tagTree = Right []

extractJeopardyRound :: TagTree Text -> Either Error JeopardyRound
extractJeopardyRound tagTree@(TagLeaf _) = Left $ "In extractJeopardyRound: Cannot extract jeopardy round from leaf node: " ++ show tagTree ++ "."
extractJeopardyRound tagTree@(TagBranch tagName attrs children) = do
    isDoubleJeopardy <- checkDoubleJeopardy
    roundTableTag <- findWithErrMsg (isElemWithAttr tableText (classText, roundText)) children
    roundTableTagChildren <- extractChildren roundTableTag
    firstTr <- findWithErrMsg (isElemType trText) roundTableTagChildren
    tableCategories <- extractTableCategories firstTr
    return $ JeopardyRound isDoubleJeopardy (Nothing, Nothing, Nothing, Nothing, Nothing, Nothing)
    where checkDoubleJeopardy :: Either Error Bool
          checkDoubleJeopardy = getId >>= isDoubleJeopardy
          
          getId :: Either Error Text 
          getId = case lookup idText attrs of
            (Just idValue) -> Right idValue
            Nothing  -> Left $ "In extractJeopardyRound: Id of tag '" ++ (show tagTree) ++ "' not found."

          isDoubleJeopardy :: Text -> Either Error Bool
          isDoubleJeopardy idValue
            | idValue == jeopardyRoundText = Right False
            | idValue == doubleJeopardyRoundText = Right True
            | otherwise = Left $ "In extractJeopardyRound: Id of tag was not either 'jeopardy_round' or 'double_jeopardy_round'. Was '" ++ (show idValue) ++ " instead."

extractFinalJeopardy :: TagTree Text -> Either Error JeopardyFinal
extractFinalJeopardy tagTree@(TagLeaf _) = Left $ "In extractFinalJeopardy: Cannot extract final jeopardy clue from leaf node: " ++ show tagTree ++ "."
extractFinalJeopardy tagTree@(TagBranch tagName attrs children) = do
    finalRoundChildren <- findWithErrMsg (isElemWithAttr tableText (classText, finalRoundText)) children >>= extractChildren
    categoryTd <- findChildOf trText (isElemWithAttr tdText (classText, categoryText)) finalRoundChildren
    clueTd <- findChildOf trText (isElemWithAttr tdText (classText, clueText)) finalRoundChildren
    
    Right $ JeopardyFinal empty empty empty

extractJeopardyGame :: [TagTree Text] -> Either Error JeopardyGame
extractJeopardyGame ts = do
    htmlTag <- findWithErrMsg (isElemType htmlText) ts
    htmlTagChildren <- extractChildren htmlTag
    
    bodyTag <- findWithErrMsg (isElemType bodyText) htmlTagChildren
    bodyTagChildren <- extractChildren bodyTag
    
    contentDiv <- findWithErrMsg (isElemWithAttr divText (idText, contentText)) bodyTagChildren
    contentDivChildren <- extractChildren contentDiv
    
    jeopardyRoundDiv <- findWithErrMsg (isElemWithAttr divText (idText, jeopardyRoundText)) contentDivChildren
    doubleJeopardyDiv <- findWithErrMsg (isElemWithAttr divText (idText, doubleJeopardyRoundText)) contentDivChildren
    finalJeopardyDiv <- findWithErrMsg (isElemWithAttr divText (idText, finalJeopardyRoundText)) contentDivChildren

    jeopardyRound <- extractJeopardyRound jeopardyRoundDiv
    doubleJeopardyRound <- extractJeopardyRound doubleJeopardyDiv
    finalJeopardy <- extractFinalJeopardy finalJeopardyDiv

    return $ JeopardyGame jeopardyRound doubleJeopardyRound finalJeopardy

isElemType :: Text -> TagTree Text -> Bool
isElemType elemType tagTree = case tagTree of
    (TagBranch str _ _) -> (toLower str) == (toLower elemType)
    (TagLeaf tag) -> case tag of
        (TagOpen str _) -> (toLower str) == (toLower elemType)
        (TagClose str)  -> (toLower str) == (toLower elemType)
        _               -> False

isElemWithAttr :: Text -> (Text, Text) -> TagTree Text -> Bool
isElemWithAttr elemType (key, val) tagTree = case tagTree of
    (TagBranch str attrs _) -> case lookup key attrs of
        (Just foundVal) -> foundVal == val && str == elemType
        Nothing    -> False
    (TagLeaf tag) -> case tag of
        (TagOpen str attrs) -> case lookup key attrs of
            (Just foundVal)   -> foundVal == val && str == elemType
            Nothing           -> False
        _                   -> False

isElemWithChild :: Text -> (TagTree Text -> Bool) -> TagTree Text -> Bool
isElemWithChild _ _ (TagLeaf _) = False
isElemWithChild elemType pred (TagBranch tagName attrs children) = or $ map pred children

extractChildren :: TagTree Text -> Either Error [TagTree Text]
extractChildren (TagBranch _ _ children) = Right children
extractChildren t                        = Left $ "In extractChildren: Could not find children of element '" ++ show t ++ "'."

findWithErrMsg :: (Show (t a), Foldable t) => (a -> Bool) -> t a -> Either Error a
findWithErrMsg pred list = case find pred list of
    (Just v) -> Right v
    Nothing  -> Left $ "In findWithErrMsg: The function 'find' failed for list:" ++ (show list) ++ "."

findChildOf :: Text -> (TagTree Text -> Bool) -> [TagTree Text] -> Either Error (TagTree Text)
findChildOf tagType pred tagTrees = filteredTagTrees >>= firstMatchingChild
    where filteredTagTrees = case filter (isElemType tagType) tagTrees of
            [] -> Left $ "In findChildOf: Found no tags of type '" ++ (show tagType) ++ "'. List was: " ++ (show tagTrees) ++ "."
            xs -> Right xs
          firstMatchingChild trees = do
            childrens <- sequence $ map extractChildren trees
            findWithErrMsg pred $ concat childrens
