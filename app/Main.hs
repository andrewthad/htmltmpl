{-# language OverloadedStrings #-}

module Main where

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
  BB.hPutBuilder IO.stdout (renderHtmlFragment UTF8 (H.run personTemplate examplePerson node) <> "\n")

data Person = Person
  { firstName :: !Text
  , lastName :: !Text
  , pets :: !(SmallArray Pet)
  }

data Pet = Pet
  { name :: !Text
  }

examplePerson :: Person
examplePerson = Person
  { firstName = "Alice"
  , lastName = "Scott"
  , pets = Exts.fromList
    [ Pet{name="Fluffy"}
    , Pet{name="Rover"}
    , Pet{name="Chuck"}
    ]
  }

personTemplate :: H.Template Person
personTemplate =
  H.string "FirstName" firstName
  <>
  H.string "LastName" lastName
  <>
  H.array "Pets" pets petTemplate

petTemplate :: H.Template Pet
petTemplate =
  H.string "Name" name
