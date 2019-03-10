{-# LANGUAGE OverloadedStrings #-}

module Syntax.Parser.Lexer (lexer, scan) where

import Syntax.Parser.Type

import Control.Monad.State
import Control.Monad.Except
import Data.Char
import Data.Loc
import Data.Text (Text, pack)
import Language.Lexer.Applicative
import Text.Regex.Applicative

matchWhen :: (s -> Bool) -> a -> RE s a
matchWhen p symbol = msym (\t -> if p t then Just symbol else Nothing)

--------------------------------------------------------------------------------


tokenRE :: RE Char Token
tokenRE
  =   TokenDefn         <$ "="
  <|> TokenHasType      <$ ":"
  -- type
  <|> TokenVarPrefix    <$ "$"
  <|> TokenDual         <$ "^"
  <|> TokenTimes        <$ "*"
  <|> TokenPar          <$ "%"
  <|> TokenPlus         <$ "+"
  <|> TokenWith         <$ "&"
  <|> TokenAcc          <$ "!"
  <|> TokenReq          <$ "?"
  <|> TokenExists       <$ "exists"
  <|> TokenForall       <$ "forall"
  <|> TokenOne          <$ "1"
  <|> TokenBot          <$ "Bot"
  <|> TokenZero         <$ "0"
  <|> TokenTop          <$ "Top"

  -- term
  <|> TokenLink         <$ "<->"
  <|> TokenScope        <$ "\\"
  <|> TokenSeq          <$ "."
  <|> TokenComp         <$ "|"
  <|> TokenParenStart   <$ "("
  <|> TokenParenEnd     <$ ")"
  <|> TokenBracketStart <$ "["
  <|> TokenBracketEnd   <$ "]"
  <|> TokenSelectL      <$ "[inl]"
  <|> TokenSelectR      <$ "[inr]"
  <|> TokenCase         <$ "case"
  <|> TokenCaseSep      <$ ","
  <|> TokenEmptyOutput  <$ "[]"
  <|> TokenEnd          <$ "end"
  <|> TokenEmptyInput   <$ "()"
  <|> TokenEmptyChoice  <$ "case()"
  <|> TokenTypeName     <$> typeName
  <|> TokenTermName     <$> termName
  <|> TokenInt          <$> intRE

(+++) :: RE Char String -> RE Char String -> RE Char String
(+++) = liftA2 (++)

-- starts with lowercase alphabets
termName :: RE Char Text
termName = fmap pack $ (:) <$> psym isLower <*> many (psym (\c -> isAlphaNum c || c == '_' || c == '\''))

-- starts with uppercase alphabets
typeName :: RE Char Text
typeName = fmap pack $ (:) <$> psym isUpper <*> many (psym (\c -> isAlphaNum c || c == '_' || c == '\''))

-- all uppercase alphabets
labelRE :: RE Char Text
labelRE = fmap pack $ (:) <$> psym isUpper <*> many (psym (\c -> isUpper c || isDigit c || c == '_' || c == '\''))

-- stringRE :: RE Char Text
-- stringRE = fmap pack $ string "\"" +++ firstPart +++ secondPart +++ string "\""
--   where
--     -- zero or more characters other than \ and "
--     firstPart :: RE Char String
--     firstPart = many (psym (\c -> c /= '\\' && c /= '\"'))
--
--     -- zero or more sequences of (`secondPart1` and then `firstPart`)
--     secondPart :: RE Char String
--     secondPart = reFoldl Greedy (++) "" (secondPart1 +++ firstPart)
--
--     -- a backslash followed with any character but a newline
--     secondPart1 :: RE Char String
--     secondPart1 = string "\\" +++ msym (\c -> if c == '\n' then Nothing else Just [c])

intRE :: RE Char Int
intRE = read <$> some (psym isDigit)

whitespaceRE :: RE Char Token
whitespaceRE = matchWhen isSpace TokenWhitespace

commentStartRE :: RE Char String
commentStartRE = string "--"

commentEndRE :: String -> RE Char Token
commentEndRE pref = TokenComment <$> fmap pack (pure pref +++ many anySym +++ string "\n")

lexer :: Lexer Token
lexer = mconcat
  [ token       (longest tokenRE)
  , whitespace  (longest whitespaceRE)
  , whitespace  (longestShortest commentStartRE commentEndRE)
  ]

--------------------------------------------------------------------------------

scan :: (Token -> Parser a) -> Parser a
scan f = scanNext >>= f

scanNext :: Parser Token
scanNext = do
  result <- gets tokenStream
  oldLoc <- gets lookaheadLoc
  case result of
    TsToken (L newLoc tok) stream -> do
      put $ ParserState oldLoc newLoc stream
      return tok
    TsEof -> do
      modify $ \st -> st { currentLoc = oldLoc , lookaheadLoc = NoLoc}
      return TokenEOF
    TsError (LexicalError pos) -> do
      throwError $ Lexical pos
