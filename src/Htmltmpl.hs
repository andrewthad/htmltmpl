{-# language DeriveAnyClass #-}
{-# language OverloadedStrings #-}

module Htmltmpl
  ( Substitutions
  , compile
  , evaluate
    -- * Build Substitutions
  , string
  , array
  , integer
  ) where

import Data.Text (Text)
import Data.Primitive (SmallArray)
import Text.XmlHtml (Node(..))
import Data.Map.Strict (Map)
import Control.Exception (Exception)
import Control.Monad (when, join)
import Text.Read (readMaybe)
import Data.List.Split (chunksOf)

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.List as List
import qualified GHC.Exts as Exts

newtype Substitutions a = Substitutions (Map Text (Field a))

instance Semigroup (Substitutions a) where
  Substitutions x <> Substitutions y = Substitutions (Map.union x y)

data Field a
  = String (a -> Text)
  | Integer (a -> Integer)
  | forall b. Array (a -> SmallArray b) (Substitutions b)

data AppliedTemplate = forall a. AppliedTemplate (Substitutions a) a
data AppliedTemplateForeach = forall a. AppliedTemplateForeach (Substitutions a) !(SmallArray a)

data Template
  = TemplateElement !Text ![(Text,Text)] ![Template]
  | TemplateText !Text
  | TemplateVar -- element tag: x-var
      !Text -- var name
  | TemplateForeach -- element tag: x-foreach
      !Text -- var name (must resolve to an array)
      ![Template] -- body of the foreach to be replicated
  | TemplateSelectForeach -- element tag: x-select-foreach
      ![(Text,Text)] -- preserved attributes (not interpreted)
      !Text -- var name (ref) (must resolve to an array)
      !Text -- the option that should start out selected (selected-ref) (optional, empty means none selected)
      !Text -- var used to extract option value (option-value-ref)
      ![Template] -- the body of each option, may reference children
  | TemplateForeachChunked
      !Text -- ref
      !Int -- chunk size (>=1, <=1000)
      ![Template] -- chunk reducer (includes the use of x-content)
      ![Template] -- item reducer (injects individual elements into the context)
  | TemplateContent
  | TemplateEmpty

string :: Text -> (a -> Text) -> Substitutions a
string name f = Substitutions (Map.singleton name (String f))

integer :: Text -> (a -> Integer) -> Substitutions a
integer name f = Substitutions (Map.singleton name (Integer f))

array :: Text -> (a -> SmallArray b) -> Substitutions b -> Substitutions a
array name f t = Substitutions (Map.singleton name (Array f t))

fieldToText :: Field a -> a -> Text
fieldToText field a = case field of
  String f -> f a
  Integer f -> Text.pack (show (f a))

lookupAsText :: [AppliedTemplate] -> Text -> Text
lookupAsText ts name = case ts of
  [] -> "unrecognized name: " <> name
  AppliedTemplate (Substitutions m) v : ts' -> case Map.lookup name m of
    Nothing -> lookupAsText ts' name
    Just field -> fieldToText field v

lookupArrayTemplate :: [AppliedTemplate] -> Text -> AppliedTemplateForeach
lookupArrayTemplate ts name = case ts of
  [] -> error "lookupArrayTemplate: could not find the thing"
  AppliedTemplate (Substitutions m) v : ts' -> case Map.lookup name m of
    Nothing -> lookupArrayTemplate ts' name
    Just field -> case field of
      Array f tmpl -> AppliedTemplateForeach tmpl (f v)
      _ -> error "lookupArrayTemplate: field was not array"

lookupAttr :: Text -> [(Text,Text)] -> Text
lookupAttr name attrs = case List.lookup name attrs of
  Nothing -> Text.empty
  Just val -> val

substituteWorker :: [Node] -> [AppliedTemplate] -> Template -> [Node]
substituteWorker content ctx node = case node of
  TemplateContent -> content
  TemplateVar ref -> [TextNode $! lookupAsText ctx ref]
  TemplateForeachChunked ref sz forChunk forItem -> case lookupArrayTemplate ctx ref of
    AppliedTemplateForeach substs vals ->
      let chunks = map
            (\chunk -> substituteWorker
              (chunk >>= (\v -> substituteWorker content (AppliedTemplate substs v : ctx) =<< forItem))
              ctx
              =<<
              forChunk
            )
            (chunksOf sz (Exts.toList vals))
       in join chunks
  TemplateSelectForeach selectAttrs ref selectedRef optionValueRef optionContentTmpls -> case lookupArrayTemplate ctx ref of
    AppliedTemplateForeach substs vals ->
      let selectedAsText = lookupAsText ctx selectedRef in
      [ Element "select" selectAttrs $ map
        (\val ->
          let ctx' = AppliedTemplate substs val : ctx
              attrs1 = [("value", lookupAsText ctx' optionValueRef)]
              attrs2 = if not (Text.null selectedAsText) && lookupAsText ctx' optionValueRef == selectedAsText
                then ("selected","selected") : attrs1
                else attrs1
           in Element "option"
                attrs2
                (substituteWorker content ctx' =<< optionContentTmpls)
        )
        (Exts.toList vals)
      ]
  TemplateForeach ref children -> case lookupArrayTemplate ctx ref of
    AppliedTemplateForeach tmpl vals ->
      Exts.toList vals >>= (\v -> substituteWorker content (AppliedTemplate tmpl v : ctx) =<< children)
  TemplateElement tag attrs children -> [Element tag attrs (substituteWorker content ctx =<< children)]
  TemplateText t -> [TextNode t]
  TemplateEmpty -> []

evaluate :: Substitutions a -> a -> Template -> [Node]
evaluate tmpl v node = substituteWorker [] [AppliedTemplate tmpl v] node

data TemplatizeError
  = VarChildrenPresent
  | VarMissingRefAttribute
  | SelectForeachMissingChildren
  | SelectForeachMissingRefAttribute
  | SelectForeachMissingOptionValueRef
  | ForeachMissingChildren
  | ForeachMissingRefAttribute
  | UnrecognizedControlTag !Text
  | ForeachChunkedIncorrectChildren
  | ForeachChunkedMissingRefAttribute
  | ForeachChunkedMissingChildren
  | ForeachChunkedMalformedChunkSizeAttribute
  | ForeachChunkedMissingChunkSizeAttribute

deriving stock instance Show TemplatizeError
deriving anyclass instance Exception TemplatizeError

-- | Build a template from a node. This interprets all control elements.
-- Control elements have tag names that begin with @x-@.
compile :: Node -> Either TemplatizeError Template
compile = nodeToTemplate

nodeToTemplate :: Node -> Either TemplatizeError Template
nodeToTemplate node = case node of
  Element tag attrs children -> case Text.stripPrefix "x-" tag of
    Just suffix -> case suffix of
      "content" -> pure TemplateContent
      "var" -> case children of
        [] -> let ref = lookupAttr "ref" attrs in case Text.null ref of
          True -> Left VarMissingRefAttribute
          False -> Right (TemplateVar ref)
        _ -> Left VarChildrenPresent
      "foreach" -> case children of
        [] -> Left ForeachMissingChildren
        _ -> let ref = lookupAttr "ref" attrs in case Text.null ref of
          True -> Left ForeachMissingRefAttribute
          False -> do
            children' <- traverse nodeToTemplate children
            Right (TemplateForeach ref children')
      "foreach-chunked" -> do
        let ref = lookupAttr "ref" attrs
        when (Text.null ref) (Left ForeachChunkedMissingRefAttribute)
        let szText = lookupAttr "chunk-size" attrs
        when (Text.null szText) (Left ForeachChunkedMissingChunkSizeAttribute)
        sz <- maybe (Left ForeachChunkedMalformedChunkSizeAttribute) Right (readMaybe (Text.unpack szText))
        case List.filter (not . isWhitespaceNode) children of
          [] -> Left ForeachChunkedMissingChildren
          [Element{elementTag="x-chunk",elementChildren=chunkChildren},Element{elementTag="x-item",elementChildren=itemChildren}] -> do
            chunkChildren' <- traverse nodeToTemplate chunkChildren
            itemChildren' <- traverse nodeToTemplate itemChildren
            pure (TemplateForeachChunked ref sz chunkChildren' itemChildren')
          _ -> Left ForeachChunkedIncorrectChildren
      "select-foreach" -> case children of
        [] -> Left SelectForeachMissingChildren
        _ -> do
          let ref = lookupAttr "ref" attrs
          when (Text.null ref) (Left SelectForeachMissingRefAttribute)
          let selectedRef = lookupAttr "selected-ref" attrs
          let optionValueRef = lookupAttr "option-value-ref" attrs
          when (Text.null optionValueRef) (Left SelectForeachMissingOptionValueRef)
          children' <- traverse nodeToTemplate children
          Right (TemplateSelectForeach (removeMagicAttributes attrs) ref selectedRef optionValueRef children')
      _ -> Left (UnrecognizedControlTag suffix)
    Nothing -> TemplateElement tag attrs <$> traverse nodeToTemplate children
  TextNode content -> Right (TemplateText content)
  Comment{} -> Right TemplateEmpty

isWhitespaceNode :: Node -> Bool
isWhitespaceNode = \case
  Comment{} -> True
  TextNode t -> Text.null (Text.strip t)
  _ -> False

removeMagicAttributes :: [(Text,Text)] -> [(Text,Text)]
removeMagicAttributes xs = case xs of
  [] -> []
  pair@(k,_) : ys -> case k of
    "ref" -> removeMagicAttributes ys
    "selected-ref" -> removeMagicAttributes ys
    "option-value-ref" -> removeMagicAttributes ys
    _ -> pair : removeMagicAttributes ys
