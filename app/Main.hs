{-# language OverloadedStrings #-}

module Main where

import Control.Exception (throwIO)
import Data.Text (Text)
import Text.XmlHtml (Document(..), Node(TextNode), parseHTML, renderHtmlFragment)
import Text.XmlHtml (Encoding(UTF8))
import System.Environment (getArgs)
import Data.Primitive (SmallArray)

import qualified GHC.Exts as Exts
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified System.IO as IO
import qualified Htmltmpl as H

main :: IO ()
main = do
  filename <- getArgs >>= \case
    [filename] -> pure filename
    _ -> fail "Expecting exactly one argument: the filename"
  contents <- B.readFile filename
  doc <- either fail pure (parseHTML filename contents)
  node <- case doc of
    HtmlDocument{docContent=nodes} -> case nodes of
      [node] -> pure node
      [node, TextNode "\n"] -> pure node -- trailing newline is fine, just strip it
      _ -> fail ("Expected a single root node but encountered " ++ show (length nodes) ++ " nodes at the root:\n" ++ show nodes)
    _ -> fail "Expected html, not xml"
  template <- either throwIO pure (H.compile node)
  BB.hPutBuilder IO.stdout (renderHtmlFragment UTF8 (H.evaluate personTemplate examplePerson template) <> "\n")

data Person = Person
  { firstName :: !Text
  , lastName :: !Text
  , favoritePet :: !Integer
  , pets :: !(SmallArray Pet)
  }

data Pet = Pet
  { ident :: !Integer
  , name :: !Text
  }

examplePerson :: Person
examplePerson = Person
  { firstName = "Alice"
  , lastName = "Scott"
  , favoritePet = 1001
  , pets = Exts.fromList
    [ Pet{ident=1000,name="Fluffy"}
    , Pet{ident=1001,name="Rover"}
    , Pet{ident=1002,name="Chuck"}
    , Pet{ident=1003,name="Lassie"}
    ]
  }

personTemplate :: H.Substitutions Person
personTemplate =
  H.string "FirstName" firstName
  <>
  H.string "LastName" lastName
  <>
  H.integer "FavoritePet" favoritePet
  <>
  H.array "Pets" pets petTemplate

petTemplate :: H.Substitutions Pet
petTemplate =
  H.integer "PetId" ident
  <>
  H.string "PetName" name
