module HtmlUtils where

import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree
import Data.Text(Text, toLower)

import Utils

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

findChildOf :: Text -> (TagTree Text -> Bool) -> [TagTree Text] -> Either Error (TagTree Text)
findChildOf tagType pred tagTrees = filteredTagTrees >>= firstMatchingChild
    where filteredTagTrees = case filter (isElemType tagType) tagTrees of
            [] -> Left $ "In findChildOf: Found no tags of type '" ++ (show tagType) ++ "'. List was: " ++ (show tagTrees) ++ "."
            xs -> Right xs
          firstMatchingChild trees = do
            childrens <- sequence $ map extractChildren trees
            findWithErrMsg pred $ concat childrens
