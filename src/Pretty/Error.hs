{-# LANGUAGE OverloadedStrings                  #-}
{-# LANGUAGE FlexibleInstances                  #-}

module Pretty.Error where

import           Syntax.Parser.Type
import           TypeChecking.Base
import           Pretty.Base
-- import Pretty.Syntax.Binding ()
import           Pretty.Syntax.Concrete         ( )
import           Base
import           Data.Text                      ( Text )
import           Data.Loc                       ( locOf )
-- import qualified Data.Set as Set
import qualified Data.Map                      as Map

import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Terminal


--------------------------------------------------------------------------------
-- |

instance Report ParseError where
  report (Lexical src pos) =
    reportS [H1 "Lexical parse error", CODE $ locOf pos] (Just src)
  report (Syntatical src loc _) =
    reportS [H1 "Syntatical parse error", CODE $ loc] (Just src)


instance Report ScopeError where
  reportS (TypeSigDuplicated a b) =
    reportS [H1 "Duplicating type signature", CODE $ locOf a, CODE $ locOf b]
  reportS (TermDefnDuplicated a b) =
    reportS [H1 "Duplicating term definition", CODE $ locOf a, CODE $ locOf b]
  reportS (DefnNotFound name) = reportS
    [ H1 "Definition not found"
    , P
    $  paint name
    <> " is not in scope"
    <> line
    <> "when checking the following call"
    , CODE $ locOf name
    ]
  reportS (RecursiveCall names) =
    reportS [H1 "Recursive calls detected", P $ pretty names]
  reportS (Others msg) =
    reportS [H1 "Other unformatted type errors", P $ pretty msg]

paint :: Pretty a => a -> Doc AnsiStyle
paint = annotate (colorDull Blue) . pretty

paint' :: Doc AnsiStyle -> Doc AnsiStyle
paint' = annotate (colorDull Blue)

instance Report TypeError where
  reportS (ChannelNotComsumed term channel) = reportS
    [ H1 "Channel occur free"
    , P
    $   "the channel"
    <+> paint channel
    <+> "should not occur free"
    <>  line
    <>  "in the following term"
    , CODE $ locOf term
    ]
  -- reportS (ChanNotFound term channel) = reportS
  --   [ H1 "Channel not found"
  --   , P $ "the channel" <+> paint channel <+> "should occur free"
  --       <> line
  --       <> "in the following term"
  --   , CODE $ locOf term
  --   ]
  -- reportS (General msg) = reportS [H1 "Other unformatted inference errors", P $ pretty msg]
  -- reportS (CannotCloseChannel term chan) = reportS
  --   [ H1 "Channel cannot be closed"
  --   , P $ "channel" <+> paint chan <+> "cannot be closed"
  --   , CODE $ locOf chan
  --   , P $ "because it is being used in the following term"
  --   , CODE $ locOf term
  --   ]
  -- reportS (ChannelNotComsumed term session) = reportS
  --   [ H1 "Channel not consumed"
  --   , P $ "these channels should be comsumed"
  --       <> line
  --       <> line
  --       <> indent 2 (report session)
  --       <> line
  --       <> line
  --       <> "in the following term"
  --   , CODE $ locOf term
  --   ]
  reportS (TypeMismatch term expectedWhole actualWhole expected actual) =
    reportS
      $  [H1 "Type mismatched"]
      ++ message
      ++ [P "when checking the following term", CODE $ locOf term]
   where
    message = if expectedWhole == expected && actualWhole == actual
      then
        [ P
          $  "expected: "
          <> paint expected
          <> line
          <> "  actual: "
          <> paint actual
        ]
      else
        [ P
          $  "expected: "
          <> paint expected
          <> line
          <> "  actual: "
          <> paint actual
          <> line
          <> line
          <> "      in: "
          <> paint expectedWhole
          <> line
          <> "     and: "
          <> paint actualWhole
        ]

  reportS (SessionMismatch term expected actual) = reportS
    [ H1 "Session mismatched"
    , P
    $  "expected: "
    <> line
    <> line
    <> indent 2 (report expected)
    <> line
    <> line
    <> "actual: "
    <> line
    <> line
    <> indent 2 (report actual)
    <> line
    <> line
    <> "when checking the following term"
    , CODE $ locOf term
    ]
  reportS (SessionNotDisjoint term a b) = reportS
    [ H1 "Sessions not disjoint"
    , P
    $  "these channels appear in both sides of the session"
    <> line
    <> line
    <> indent 2 (report (Map.intersection a b))
    <> line
    <> line
    <> "when checking the following term"
    , CODE $ locOf term
    ]

  reportS (SessionNotAllRequest term session) = reportS
    [ H1 "Channels should all be requesting"
    , P
    $  "there are some channels"
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



instance Report RuntimeError where
  report (Runtime_NotInScope name) =
    report [H1 "Process not defined", P $ paint name <+> "is not in scope"]
  report Runtime_CodeNotLoaded = report
    [ H1 "Source not loaded yet"
    , P $ "type " <> paint (":l FILEPATH" :: Text) <> " to load the source"
    ]
  report (Runtime_CannotMatch _process groups) = report
    [ H1 "Unmatched channels"
    , P $ "These channels don't match with each other" <+> pretty groups
    -- , CODE $ locOf process
    ]
  report (Runtime_Stuck _process) =
    report [H1 "Stuck"
    -- , CODE $ locOf process
                      ]


instance Report Error where
  reportS (ParseError   err) = reportS err
  reportS (ScopeError   err) = reportS err
  reportS (TypeError    err) = reportS err
  reportS (RuntimeError err) = reportS err
  reportS (Panic        msg) = const (pretty msg)
