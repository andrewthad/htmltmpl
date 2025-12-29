{-# language DeriveAnyClass #-}
{-# language OverloadedStrings #-}

module Htmltmpl
  ( Projections
  , Template
  , compile
  , evaluate
  , checkTemplateAgainstProjections
    -- * Build Projections
  , string
  , array
  , integer
  ) where

import Data.Foldable (traverse_)
import Data.Text (Text)
import Data.Primitive (SmallArray)
import Text.XmlHtml (Node(..))
import Data.Map.Strict (Map)
import Control.Exception (Exception)
import Control.Monad (when, join)
import Text.Read (readMaybe)
import Data.List.Split (chunksOf)
import Data.Primitive (sizeofSmallArray)

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.List as List
import qualified GHC.Exts as Exts

newtype Projections a = Projections (Map Text (Field a))

instance Semigroup (Projections a) where
  Projections x <> Projections y = Projections (Map.union x y)

instance Monoid (Projections a) where
  mempty = Projections Map.empty

data Field a
  = String (a -> Text)
  | Integer (a -> Integer)
  | forall b. Array (a -> SmallArray b) (Projections b)

data AppliedTemplate = forall a. AppliedTemplate (Projections a) a
data AppliedTemplateForeach = forall a. AppliedTemplateForeach (Projections a) !(SmallArray a)

data Context = Context (Map Text Type)
deriving stock instance Show Context

data Type
  = TypeString
  | TypeInteger
  | TypeArray Context
deriving stock instance Show Type

data Template
  = TemplateElement !Text ![(Text,Text)] ![Template]
  | TemplateEnrichedElement
      !Text
      ![(Text,Text)]
      !Text -- class ref
      ![Template]
  | TemplateText !Text
  | TemplateVar -- element tag: x-var
      !Text -- var name
  | TemplateIf -- element tag: x-foreach
      !Text -- var name (must resolve to an array)
      ![Template] -- body of the foreach to be replicated
  | TemplateForeach -- element tag: x-foreach
      !Text -- var name (must resolve to an array)
      ![Template] -- body of the foreach to be replicated
  | TemplateSelectForeach -- element tag: x-select-foreach
      ![(Text,Text)] -- preserved attributes (not interpreted)
      !Text -- var name (ref) (must resolve to an array)
      !Text -- the option that should start out selected (selected-ref) (optional, empty means none selected)
      !Text -- var used to extract option value (option-value-ref)
      !Text -- var used for name field (name-ref, empty means do not use)
      ![Template] -- the body of each option, may reference children
  | TemplateForeachChunked -- Get rid of this. I don't like it.
      !Text -- ref
      !Int -- chunk size (>=1, <=1000)
      ![Template] -- chunk reducer (includes the use of x-content)
      ![Template] -- item reducer (injects individual elements into the context)
  | TemplateContent -- used by a function to include the argument content
  | TemplateEmpty

string :: Text -> (a -> Text) -> Projections a
string name f = Projections (Map.singleton name (String f))

integer :: Text -> (a -> Integer) -> Projections a
integer name f = Projections (Map.singleton name (Integer f))

array :: Text -> (a -> SmallArray b) -> Projections b -> Projections a
array name f t = Projections (Map.singleton name (Array f t))

fieldToText :: Field a -> a -> Text
fieldToText field a = case field of
  String f -> f a
  Integer f -> Text.pack (show (f a))

lookupAsText :: [AppliedTemplate] -> Text -> Text
lookupAsText ts name = case ts of
  [] -> "unrecognized name: " <> name
  AppliedTemplate (Projections m) v : ts' -> case Map.lookup name m of
    Nothing -> lookupAsText ts' name
    Just field -> fieldToText field v

lookupArrayTemplate :: [AppliedTemplate] -> Text -> AppliedTemplateForeach
lookupArrayTemplate ts name = case ts of
  [] -> error "lookupArrayTemplate: could not find the thing"
  AppliedTemplate (Projections m) v : ts' -> case Map.lookup name m of
    Nothing -> lookupArrayTemplate ts' name
    Just field -> case field of
      Array f tmpl -> AppliedTemplateForeach tmpl (f v)
      _ -> error "lookupArrayTemplate: field was not array"

lookupAttr :: Text -> [(Text,Text)] -> Text
lookupAttr name attrs = case List.lookup name attrs of
  Nothing -> Text.empty
  Just val -> val

appendToClassAttr :: Text -> [(Text, Text)] -> [(Text, Text)]
appendToClassAttr clsName attrs = case attrs of
  [] -> [("class", clsName)]
  pair@(attrName,attrVal) : ys -> case attrName of
    "class" -> ("class", attrVal <> " " <> clsName) : ys
    _ -> pair : appendToClassAttr clsName ys

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
  TemplateSelectForeach selectAttrs0 ref selectedRef optionValueRef nameRef optionContentTmpls -> case lookupArrayTemplate ctx ref of
    AppliedTemplateForeach substs vals ->
      let selectedAsText = lookupAsText ctx selectedRef in
      let selectAttrs1 = case nameRef of
            "" -> selectAttrs0
            _ -> ("name", lookupAsText ctx nameRef) : selectAttrs0
       in
      [ Element "select" selectAttrs1 $ map
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
  TemplateIf ref children -> case lookupArrayTemplate ctx ref of
    AppliedTemplateForeach _ vals -> if sizeofSmallArray vals == 0
      then []
      else substituteWorker content ctx =<< children
  TemplateEnrichedElement tag attrs classRef children ->
    let attrs' = case classRef of
          "" -> attrs
          _ -> case lookupAsText ctx classRef of
            "" -> attrs
            v -> appendToClassAttr v attrs
     in [Element tag attrs' (substituteWorker content ctx =<< children)]
  TemplateElement tag attrs children -> [Element tag attrs (substituteWorker content ctx =<< children)]
  TemplateText t -> [TextNode t]
  TemplateEmpty -> []

evaluate :: Projections a -> a -> Template -> [Node]
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
  | IfMissingChildren
  | IfMissingRefAttribute

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
      "div" -> do
        let classRef = lookupAttr "class-ref" attrs
        let attrs' = removeMagicAttributes attrs
        children' <- traverse nodeToTemplate children
        Right (TemplateEnrichedElement "div" attrs' classRef children')
      "if" -> case children of
        [] -> Left IfMissingChildren
        _ -> let ref = lookupAttr "ref" attrs in case Text.null ref of
          True -> Left IfMissingRefAttribute
          False -> do
            children' <- traverse nodeToTemplate children
            Right (TemplateIf ref children')
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
          let nameRef = lookupAttr "name-ref" attrs
          when (Text.null optionValueRef) (Left SelectForeachMissingOptionValueRef)
          children' <- traverse nodeToTemplate children
          Right (TemplateSelectForeach (removeMagicAttributes attrs) ref selectedRef optionValueRef nameRef children')
      _ -> Left (UnrecognizedControlTag suffix)
    Nothing -> TemplateElement tag attrs <$> traverse nodeToTemplate children
  TextNode content -> Right (TemplateText content)
  Comment{} -> Right TemplateEmpty

data ContextCheckError
  = MissingVar !Text
  | ExpectedVarWithScalarType
  | IfExpectsArray
      !Text -- ref name
      !Type -- ref type
  | ForeachExpectsArray
      !Text -- ref name
      !Type -- ref type
  | SelectForeachExpectsArray
      !Text -- ref name
      !Type -- ref type
deriving stock instance Show ContextCheckError
deriving anyclass instance Exception ContextCheckError

data Content = ContentYes | ContentNo

projectionsToContext :: Projections a -> Context
projectionsToContext (Projections m) = Context (fmap fieldToType m)

fieldToType :: Field a -> Type
fieldToType = \case
  String{} -> TypeString
  Integer{} -> TypeInteger
  Array _ p -> TypeArray (projectionsToContext p)

requireScalarVarInContext :: Text -> Context -> Either ContextCheckError ()
requireScalarVarInContext ref (Context ctxMap) = case Map.lookup ref ctxMap of
  Nothing -> Left (MissingVar ref)
  Just ty -> case ty of
    TypeString{} -> pure ()
    TypeInteger{} -> pure ()
    _ -> Left ExpectedVarWithScalarType

checkTemplateAgainstProjections :: Projections a -> Template -> Either ContextCheckError ()
checkTemplateAgainstProjections p tmpl = checkWorker ContentNo (projectionsToContext p) tmpl

checkWorker :: Content -> Context -> Template -> Either ContextCheckError ()
checkWorker content ctx@(Context ctxMap) tmpl = case tmpl of
  TemplateElement _ _ children -> traverse_ (checkWorker content ctx) children
  TemplateText{} -> pure ()
  TemplateSelectForeach _ ref selectedRef optionValueRef nameRef optionContentTmpls -> case Map.lookup ref ctxMap of
    Nothing -> Left (MissingVar ref)
    Just ty -> case ty of
      TypeArray elemTyCtx@(Context elemTy) -> do
        when (not (Text.null selectedRef)) (requireScalarVarInContext selectedRef ctx)
        when (not (Text.null nameRef)) (requireScalarVarInContext nameRef ctx)
        requireScalarVarInContext optionValueRef elemTyCtx
        traverse_ (checkWorker content (Context (Map.union elemTy ctxMap))) optionContentTmpls
      _ -> Left (SelectForeachExpectsArray ref ty)
  TemplateForeach ref body -> case Map.lookup ref ctxMap of
    Nothing -> Left (MissingVar ref)
    Just ty -> case ty of
      TypeArray (Context elemTy) -> traverse_ (checkWorker content (Context (Map.union elemTy ctxMap))) body
      _ -> Left (ForeachExpectsArray ref ty)
  TemplateIf ref body -> case Map.lookup ref ctxMap of
    Nothing -> Left (MissingVar ref)
    Just ty -> case ty of
      TypeArray _ -> traverse_ (checkWorker content ctx) body
      _ -> Left (IfExpectsArray ref ty)
  TemplateEnrichedElement _ attrs classRef children -> do
    when (not (Text.null classRef)) (requireScalarVarInContext classRef ctx)
    traverse_ (checkWorker content ctx) children
  TemplateVar ref -> case Map.lookup ref ctxMap of
    Nothing -> Left (MissingVar ref)
    Just{} -> pure ()

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
    "name-ref" -> removeMagicAttributes ys
    "class-ref" -> removeMagicAttributes ys
    _ -> pair : removeMagicAttributes ys
