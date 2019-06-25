{-# LANGUAGE OverloadedStrings                  #-}
{-# LANGUAGE TypeSynonymInstances               #-}
{-# LANGUAGE FlexibleInstances                  #-}

module Pretty.Error where

import Syntax.Parser.Type
import TypeChecking.Base
import Pretty.Base
import Pretty.Syntax.Binding ()
import Pretty.Syntax.Concrete ()
import Base

import Data.Monoid ((<>))
import Data.Text.Prettyprint.Doc hiding (line)
import Data.Text (Text)
import Data.Loc (locOf)

import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal


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


instance Report ScopeError where
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
  reportS (Others msg) = reportS
    [ H1 "Other unformatted type errors"
    , P $ pretty msg
    ]

highlight :: Pretty a => a -> Doc AnsiStyle
highlight = annotate (colorDull Blue) . pretty

instance Report TypeError where
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
  report (Runtime_CannotMatch comm x y) = report
    [ H1 "Unmatched channels"
    , P $ pretty x <+> "doesn't match with" <+> pretty y
    , CODE $ locOf comm
    ]
  report (Runtime_Stuck process) = report
    [ H1 "Stuck"
    , CODE $ locOf process
    ]


instance Report Error where
  reportS (ParseError err) = reportS err
  reportS (TypeError err) = reportS err
  reportS (RuntimeError err) = reportS err
  reportS (Panic msg) = const (pretty msg)
