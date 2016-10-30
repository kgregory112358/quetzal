module Jeopardy where

import Text.HTML.TagSoup.Tree
import Data.Text(Text, empty)

import TextConstants
import HtmlUtils
import Utils

data JeopardyClue = JeopardyClue Text Text deriving (Show, Eq)
data JeopardyCategory = JeopardyCategory Text (Tuple5 (Maybe JeopardyClue)) deriving (Show, Eq)
data JeopardyRound = JeopardyRound Bool (Tuple6 (Maybe JeopardyCategory)) deriving (Show, Eq)
data JeopardyFinal = JeopardyFinal Text Text Text deriving (Show, Eq)
data JeopardyGame = JeopardyGame JeopardyRound JeopardyRound JeopardyFinal deriving (Show, Eq)

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

