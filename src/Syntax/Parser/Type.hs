module Syntax.Parser.Type where


import Control.Monad.State
import Control.Monad.Except
import Data.Loc
import Data.Text (Text)

import Language.Lexer.Applicative

--------------------------------------------------------------------------------
-- | Tokens

data Token
  -- Name = Process
  = TokenDefn
  | TokenName Text
  -- <->
  | TokenLink
  -- \x.(P | Q)
  | TokenScope    -- \
  | TokenSeq      -- .
  | TokenComp  -- |
  -- () []
  | TokenParenStart | TokenParenEnd
  | TokenBracketStart | TokenBracketEnd
  -- [inl] [inr]
  | TokenSelectL
  | TokenSelectR
  -- x.case(P, Q)
  | TokenCase     -- case
  | TokenCaseSep  -- ,
  -- !x(y).P
  | TokenAccept   -- !
  -- ?x[y].P
  | TokenRequest  -- ?
  -- x[].0
  | TokenEmptyOutput
  | TokenEnd
  -- x().P
  | TokenEmptyInput
  -- x.case()
  | TokenEmptyChoice

  -- whitespace, comments amd EOF
  | TokenComment Text
  | TokenWhitespace
  | TokenEOF

  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- | ParseError and stuff

data ParseError = Lexical Pos | Syntatical Loc Token
  deriving (Show)

data ParserState = ParserState
  { currentLoc :: Loc
  , lookaheadLoc :: Loc
  , tokenStream :: TokenStream (L Token)
  } deriving (Show)

type Parser = StateT ParserState (Except ParseError)

syntaticalError :: Token -> Parser a
syntaticalError tok = do
  loc <- gets lookaheadLoc
  throwError $ Syntatical loc tok

locate :: (Loc -> a) -> Parser a
locate f = f <$> gets currentLoc
