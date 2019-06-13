{-# LANGUAGE OverloadedStrings                  #-}
{-# LANGUAGE TypeSynonymInstances               #-}
{-# LANGUAGE FlexibleInstances                  #-}

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
-- | Report typeclass

class Report a where
  report :: a -> Doc AnsiStyle
  report a = reportS a Nothing

  reportS :: a -> Maybe ByteString -> Doc AnsiStyle
  reportS a _ = report a

data ReportMsg
  = H1 Text       -- appears red
  | P (Doc AnsiStyle)
  | CODE Loc
  deriving (Show)

instance Report ReportMsg where
  reportS (H1 s) _ = annotate (color Red) $ pretty s
  reportS (P s) _ = s <> line
  reportS (CODE loc) Nothing = annotate (colorDull Blue) (pretty $ Loc.displayLoc loc)
  reportS (CODE loc) (Just src) = indent 2 $ vsep
        [ annotate (colorDull Blue) (pretty $ Loc.displayLoc loc)
        , reAnnotate toAnsiStyle $ prettySourceCode $ SourceCode src loc 1
        ]

instance Report [ReportMsg] where
  reportS msgs src = vsep $
          [ softline' ]
      ++  map (\msg -> indent 2 $ reportS msg src <> line) msgs
      ++  [ softline' ]

--------------------------------------------------------------------------------
-- |

instance Report ParseError where
  report (Lexical src pos) = reportS
    [ H1 "Lexical parse error"
    , CODE $ locOf pos
    ] (Just src)
  report (Syntatical src loc _) = reportS
    [ H1 "Syntatical parse error"
    , CODE $ loc
    ] (Just src)


instance Report TypeError where
  reportS (TypeSigDuplicated a b) = reportS
    [ H1 "Duplicating type signature"
    , CODE $ locOf a
    , CODE $ locOf b
    ]
  reportS (TermDefnDuplicated a b) = reportS
    [ H1 "Duplicating term definition"
    , CODE $ locOf a
    , CODE $ locOf b
    ]
  reportS (InferError a) = reportS a
  reportS (Others msg) = reportS
    [ H1 "Other unformatted type errors"
    , P $ pretty msg
    ]

highlight :: Pretty a => a -> Doc AnsiStyle
highlight = annotate (colorDull Blue) . pretty

instance Report Session where
  report = pretty . Map.mapKeys toAbstract

instance Report InferError where
  reportS (General msg) = reportS [H1 "Other unformatted inference errors", P $ pretty msg]
  reportS (CannotCloseChannel term chan) = reportS
    [ H1 "Channel cannot be closed"
    , P $ "channel" <+> highlight chan <+> "cannot be closed"
    , CODE $ locOf chan
    , P $ "because it is being used in the following term"
    , CODE $ locOf term
    ]
  reportS (ChannelNotComsumed term session) = reportS
    [ H1 "Channel not consumed"
    , P $ "these channels should be comsumed"
        <> line
        <> line
        <> indent 2 (report session)
        <> line
        <> line
        <> "in the following term"
    , CODE $ locOf term
    ]
  reportS (TypeMismatch term expectedWhole actualWhole expected actual) = reportS $
        [ H1 "Type mismatched" ]
    ++  message
    ++  [ P "when checking the following term"
        , CODE $ locOf term
        ]
    where message = if expectedWhole == expected && actualWhole == actual
            then
              [ P $  "expected: " <> highlight expected <> line
                  <> "  actual: " <> highlight actual
              ]
            else
              [ P $  "expected: " <> highlight expected <> line
                  <> "  actual: " <> highlight actual    <> line
                  <> line
                  <> "      in: " <> highlight expectedWhole <> line
                  <> "     and: " <> highlight actualWhole
              ]

  reportS (SessionMismatch term expected actual) = reportS
    [ H1 "Session mismatched"
    , P $ "expected: "
        <> line
        <> line
        <> indent 2 (report expected)
        <> line
        <> line
        <>  "actual: "
        <> line
        <> line
        <> indent 2 (report actual)
        <> line
        <> line
        <>  "when checking the following term"
    , CODE $ locOf term
    ]

  reportS (SessionShouldAllBeRequesting term session) = reportS
    [ H1 "Channels should all be requesting"
    , P $ "there are some channels"
        <> line
        <> "that are not requesting anything"
        <> line
        <> line
        <> indent 2 (report session)
        <> line
        <> line
        <> "when checking the following term"
    , CODE $ locOf term
    ]

  reportS (DefnNotFound term name) = reportS
    [ H1 "Definition not found"
    , P $ highlight name <> " is not in scope"
        <> line
        <> "when checking the following term"
    , CODE $ locOf term
    ]

  reportS (SessionShouldBeDisjoint term session) = reportS
    [ H1 "Sessions not disjoint"
    , P $ "these channels appear in both sides of the session" <> line
        <> line
        <> indent 2 (report session)
        <> line
        <> line
        <> "when checking the following term"
    , CODE $ locOf term
    ]

instance Report RuntimeError where
  report (Runtime_NotInScope name) = report
    [ H1 "Process not defined"
    , P $ highlight name <+> "is not in scope"
    ]
  report Runtime_CodeNotLoaded = report
    [ H1 "Source not loaded yet"
    , P $ "type " <> highlight (":l FILEPATH" :: Text) <> " to load the source"
    ]

instance Report Error where
  reportS (ParseError err) = reportS err
  reportS (TypeError err) = reportS err
  reportS (RuntimeError err) = reportS err
  reportS (Panic msg) = const (pretty msg)
