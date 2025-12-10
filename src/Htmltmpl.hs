{-# language OverloadedStrings #-}

module Htmltmpl
  ( Template
  , string
  , array
  , run
  ) where

import Data.Text (Text)
import Data.Primitive (SmallArray)
import Text.XmlHtml (Node(..))
import Data.Map.Strict (Map)

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.List as List
import qualified GHC.Exts as Exts

newtype Template a = Template (Map Text (Field a))

instance Semigroup (Template a) where
  Template x <> Template y = Template (Map.union x y)

data Field a
  = String (a -> Text)
  | Integer (a -> Integer)
  | forall b. Array (a -> SmallArray b) (Template b)

data AppliedTemplate = forall a. AppliedTemplate (Template a) a
data AppliedTemplateForeach = forall a. AppliedTemplateForeach (Template a) !(SmallArray a)

string :: Text -> (a -> Text) -> Template a
string name f = Template (Map.singleton name (String f))

array :: Text -> (a -> SmallArray b) -> Template b -> Template a
array name f t = Template (Map.singleton name (Array f t))

fieldToText :: Field a -> a -> Text
fieldToText field a = case field of
  String f -> f a

lookupAsText :: [AppliedTemplate] -> Text -> Text
lookupAsText ts name = case ts of
  [] -> "unrecognized name: " <> name
  AppliedTemplate (Template m) v : ts' -> case Map.lookup name m of
    Nothing -> lookupAsText ts' name
    Just field -> fieldToText field v

lookupArrayTemplate :: [AppliedTemplate] -> Text -> AppliedTemplateForeach
lookupArrayTemplate ts name = case ts of
  [] -> error "lookupArrayTemplate: could not find the thing"
  AppliedTemplate (Template m) v : ts' -> case Map.lookup name m of
    Nothing -> lookupArrayTemplate ts' name
    Just field -> case field of
      Array f tmpl -> AppliedTemplateForeach tmpl (f v)
      _ -> error "lookupArrayTemplate: field was not array"

lookupAttr :: Text -> [(Text,Text)] -> Text
lookupAttr name attrs = case List.lookup name attrs of
  Nothing -> Text.empty
  Just val -> val

substituteWorker :: [AppliedTemplate] -> Node -> [Node]
substituteWorker ctx node = case node of
  Element tag attrs children -> case tag of
    "x-var" -> case children of
      [] -> let ref = lookupAttr "ref" attrs in case Text.null ref of
        True -> [TextNode "x-var has empty or missing ref"]
        False -> [TextNode $! lookupAsText ctx ref]
      _ -> [TextNode "x-var must not have children"]
    "x-foreach" -> case children of
      [] -> [TextNode "x-foreach must have children"]
      _ -> let ref = lookupAttr "ref" attrs in case Text.null ref of
        True -> [TextNode "x-foreach has empty or missing ref"]
        False -> case lookupArrayTemplate ctx ref of
          AppliedTemplateForeach tmpl vals ->
            Exts.toList vals >>= (\v -> substituteWorker (AppliedTemplate tmpl v : ctx) =<< children)
    _ -> [Element tag attrs (substituteWorker ctx =<< children)]
  _ -> [node] -- leave text nodes and comments alone

run :: Template a -> a -> Node -> [Node]
run tmpl v node = substituteWorker [AppliedTemplate tmpl v] node

someFunc :: IO ()
someFunc = putStrLn "someFunc"
