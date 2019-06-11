{-# LANGUAGE OverloadedStrings                  #-}

module Pretty.Error where

import Syntax.Parser.Type
import Syntax.Concrete (ToAbstract(..))
import TypeChecking.Base
import Pretty
import Pretty.Syntax.Concrete ()
import Base

import Data.Monoid ((<>))
import Data.ByteString.Lazy (ByteString)
import Data.Text.Prettyprint.Doc hiding (line)
import Data.Text (Text)
import Data.Loc (Loc(..), locOf)
import qualified Data.Loc as Loc
import qualified Data.Map as Map

import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal

--------------------------------------------------------------------------------
-- |


formatError :: Text             -- Header
            -> [Doc AnsiStyle]  -- Description
            -> [Loc]            -- List of locations to highlight
            -> Maybe ByteString -- Source code to be highlighted
            -> Doc AnsiStyle
formatError header paragraphs locations Nothing =
  vsep
    [ softline'
    , indent 2 pieces
    , indent 2 text
    , softline'
    ]
  where
      text = vsep $
            [ annotate (color Red) $ pretty header
            , softline
            ]
            ++ (map (\ x -> x <> line) paragraphs)
      pieces :: Doc AnsiStyle
      pieces = hsep $ map (\loc -> annotate (colorDull Blue) (pretty $ Loc.displayLoc loc)) locations
formatError header paragraphs locations (Just source) =
  vsep
    [ softline'
    , indent 2 text
    , indent 4 pieces
    , softline'
    ]
  where
      text = vsep $
            [ annotate (color Red) $ pretty header
            , softline
            ]
            ++ (map (\ x -> x <> line) paragraphs)
      pieces = vsep $ map (\loc -> vsep
            [ annotate (colorDull Blue) (pretty $ Loc.displayLoc loc)
            , reAnnotate toAnsiStyle $ prettySourceCode $ SourceCode source loc 1
            ]) locations


prettyParseError :: ParseError -> Doc AnsiStyle
prettyParseError (Lexical src pos) =
  formatError "Lexical parse error" [] [locOf pos] (Just src)
prettyParseError (Syntatical src loc _) =
  formatError "Lexical parse error" [] [loc] (Just src)


prettyTypeError :: TypeError -> Maybe ByteString -> Doc AnsiStyle
prettyTypeError (TypeSigDuplicated a b) =
  formatError "Duplicating type signature" []
    [locOf a, locOf b]
prettyTypeError (TermDefnDuplicated a b) =
  formatError "Duplicating term definition" []
    [locOf a, locOf b]
prettyTypeError (InferError a) =
   prettyInferError a
prettyTypeError (Others msg) =
  formatError "Other unformatted type errors" [pretty msg] []

highlight :: Pretty a => a -> Doc AnsiStyle
highlight = annotate (colorDull Blue) . pretty

prettySession :: Session -> Doc AnsiStyle
prettySession = pretty . Map.mapKeys toAbstract

prettyInferError :: InferError -> Maybe ByteString -> Doc AnsiStyle
prettyInferError (General msg) = formatError "Other unformatted inference errors" [pretty msg] []
prettyInferError (CannotAppearInside term chan) =
  formatError "Channel not allowed"
    [ "channel "
        <> highlight chan <> " is not allowed"
        <> line
        <> "to appear in the following term"
    ] [locOf term]
prettyInferError (ChannelNotComsumed term session) =
  formatError "Channel not consumed"
    [ "these channels should be comsumed"
        <> line
        <> line
        <> indent 2 (prettySession session)
        <> line
        <> line
        <> "in the following term"
    ] [locOf term]
prettyInferError (TypeMismatch term expectedWhole actualWhole expected actual) =
  formatError "Type mismatched"
    (message ++
    [   "when checking the following term"
    ]) [locOf term]
    where message = if expectedWhole == expected && actualWhole == actual
            then
              [      "expected: " <> highlight expected <> line
                  <> "  actual: " <> highlight actual
              ]
            else
              [      "expected: " <> highlight expected <> line
                  <> "  actual: " <> highlight actual    <> line
                  <> line
                  <> "      in: " <> highlight expectedWhole <> line
                  <> "     and: " <> highlight actualWhole
              ]

prettyInferError (SessionMismatch term expected actual) =
  formatError "Session mismatched"
    [
          "expected: "
      <> line
      <> line
      <> indent 2 (prettySession expected)
      <> line
      <> line
      <>  "actual: "
      <> line
      <> line
      <> indent 2 (prettySession actual)
      <> line
      <> line
      <>  "when checking the following term"
    ] [locOf term]

prettyInferError (SessionShouldAllBeRequesting term session) =
  formatError "Channels should all be requesting"
    [ "there are some channels"
        <> line
        <> "that are not requesting anything"
        <> line
        <> line
        <> indent 2 (prettySession session)
        <> line
        <> line
        <> "when checking the following term"
    ] [locOf term]

prettyInferError (DefnNotFound term name) =
  formatError "Definition not found"
    [ highlight name <> " is not in scope"
        <> "when checking the following term"
    ] [locOf term]

prettyInferError (SessionShouldBeDisjoint term session) =
  formatError "Sessions not disjoint"
    [ "these channels appear in both sides of the session" <> line
        <> line
        <> indent 2 (prettySession session)
        <> line
        <> line
        <> "when checking the following term"
    ] [locOf term]

prettyInferError e = formatError "" [pretty $ show $ e] []

prettyRuntimeError :: RuntimeError -> Doc AnsiStyle
prettyRuntimeError (Runtime_NotInScope name) =
  formatError "Process not defined"
    [ highlight name <+> "is not in scope"
    ] [] Nothing
prettyRuntimeError Runtime_CodeNotLoaded =
  formatError "Source not loaded yet"
    [ "type " <> highlight (":l FILEPATH" :: Text) <> " to load the source"
    ] [] Nothing

prettyError :: Error -> Maybe ByteString -> Doc AnsiStyle
prettyError err source = do
  case err of
    ParseError parseError -> prettyParseError parseError
    TypeError typeError -> prettyTypeError typeError source
    RuntimeError runtimeError -> prettyRuntimeError runtimeError
    Panic msg -> pretty msg
