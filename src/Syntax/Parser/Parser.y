{
{-# LANGUAGE OverloadedStrings                  #-}

module Syntax.Parser.Parser where

import Syntax.Parser.Lexer
import Syntax.Parser.Type
import Syntax.Concrete
import Syntax.Base
import qualified Data.Map as Map
import Data.Loc hiding (Pos)
import Prelude hiding (GT, LT, EQ)

import Data.Text (Text)

}

%name programParser Program
%name processParser ProcessMix
%name sessionSyntaxParser SessionSyntax

%tokentype { Token }
%error { syntaticalError }

%monad { Parser }
%lexer { scan } { TokenEOF }

%token
        termName        { TokenTermName $$      }
        typeName        { TokenTypeName $$      }
        '='             { TokenDefn             }
        ':'             { TokenHasType          }
        '^'             { TokenDual             }
        '*'             { TokenTimes            }
        '%'             { TokenPar              }
        '+'             { TokenPlus             }
        '&'             { TokenWith             }
        '!'             { TokenAcc              }
        '?'             { TokenReq              }
        'exists'        { TokenExists           }
        'forall'        { TokenForall           }
        '1'             { TokenOne              }
        'Bot'           { TokenBot              }
        '0'             { TokenZero             }
        'Top'           { TokenTop              }
        '{}'            { TokenEmptySession     }

        '<->'           { TokenLink             }
        'nu'            { TokenScope            }
        '.'             { TokenSeq              }
        '|'             { TokenComp             }
        '('             { TokenParenStart       }
        ')'             { TokenParenEnd         }
        '['             { TokenBracketStart     }
        ']'             { TokenBracketEnd       }
        '[inl]'         { TokenSelectL          }
        '[inr]'         { TokenSelectR          }
        'case'          { TokenCase             }
        ','             { TokenCaseSep          }
        '[]'            { TokenEmptyOutput      }
        'end'           { TokenEnd              }
        '()'            { TokenEmptyInput       }
        'case()'        { TokenEmptyChoice      }

%right '.'

%%

s :: { Loc }
    : {- empty -}                               {% getLoc }

Program :: {Program}
    : {- empty -}                               {% locate $ Program [] }
    | Declarations                              {% locate $ Program (reverse $1) }

-- left recursive
Declarations :: {[Declaration]}
    : Declaration                               { [$1] }
    | Declarations Declaration                  { $2:$1 }

Declaration :: {Declaration}
    : s Name ':' SessionSyntax                  {% locate' $1 $ TypeSig $2 $4 }
    | s Name '=' ProcessMix                     {% locate' $1 $ TermDefn $2 $4 }

-- left recursive
ProcessMix :: {Process}
    : ProcessMix '|' Process                    {% locate $ Mix $1 $3 }
    | Process                                   { $1 }

Process :: {Process}
    : s Name                                                {% locate' $1 $ Call $2  }
    | s Chan '<->' Chan                                     {% locate' $1 $ Link $2 $4  }
    | s 'nu' Chan ':' Type '.' '(' Process '|' Process ')'  {% locate' $1 $ Compose $3 (Just $5) $8 $10 }
    | s 'nu' Chan          '.' '(' Process '|' Process ')'  {% locate' $1 $ Compose $3 Nothing $6 $8 }
    | s Chan '[' Chan ']'  '.' '(' Process '|' Process ')'  {% locate' $1 $ Output $2 $4 $8 $10 }
    | s Chan '(' Chan ')'  '.' Process                      {% locate' $1 $ Input $2 $4 $7 }
    | s Chan '[inl]' '.' Process                            {% locate' $1 $ SelectL $2 $5 }
    | s Chan '[inr]' '.' Process                            {% locate' $1 $ SelectR $2 $5 }
    | s Chan '.' 'case' '(' Process ',' Process ')'         {% locate' $1 $ Choice $2 $6 $8 }
    | s '!' Chan '(' Chan ')' '.' Process                   {% locate' $1 $ Accept $3 $5 $8 }
    | s '?' Chan '[' Chan ']' '.' Process                   {% locate' $1 $ Request $3 $5 $8 }
    | s Chan '[' Type ']' '.' Process                       {% locate' $1 $ OutputT $2 $4 $7 }
    | s Chan '(' TypeVar ')' '.' Process                    {% locate' $1 $ InputT $2 $4 $7 }
    | s Chan '[]' '.' 'end'                                 {% locate' $1 $ EmptyOutput $2 }
    | s Chan '()' '.' Process                               {% locate' $1 $ EmptyInput $2 $5 }
    | s Chan '.' 'case()'                                   {% locate' $1 $ EmptyChoice $2 }
    | s 'end'                                               {% locate' $1 $ End }
    | s '(' ProcessMix ')'                                  { $3 }

SessionSyntax :: {SessionSyntax}
    : s '{}'                                                {% locate' $1 $ emptySessionSyntax }
    | s Chan ':' Type                                       {% locate' $1 $ singletonSessionSyntax $2 $4 }
    | SessionSyntax ',' Chan ':' Type                       { insertSessionSyntax $3 $5 $1 }

Type :: {Type}
    : s Type1                                   { $2 }
    | s 'exists' TypeVar '.' Type1              {% locate' $1 $ Exists $3 $5 }
    | s 'forall' TypeVar '.' Type1              {% locate' $1 $ Forall $3 $5 }

-- right associative
Type1 :: {Type}
    : s Type2                                   { $2 }
    | s Type2 '*' Type1                         {% locate' $1 $ Times $2 $4 }
    | s Type2 '%' Type1                         {% locate' $1 $ Par $2 $4 }

-- right associative
Type2 :: {Type}
    : s Type3                                   { $2 }
    | s Type3 '+' Type2                         {% locate' $1 $ Plus $2 $4 }
    | s Type3 '&' Type2                         {% locate' $1 $ With $2 $4 }

Type3 :: {Type}
    : s Type4                                   { $2 }
    | s '!' Type3                               {% locate' $1 $ Acc $3 }
    | s '?' Type3                               {% locate' $1 $ Req $3 }

Type4 :: {Type}
    : '(' Type ')'                              { $2 }
    | s '^' Type4                               {% locate' $1 $ Dual $3  }
    | '1'                                       {% locate $ One }
    | 'Bot'                                     {% locate $ Bot }
    | '0'                                       {% locate $ Zero }
    | 'Top'                                     {% locate $ Top }
    | TypeVar                                   {% locate $ Var $1 }

Name :: {Name}
    : termName                                  {% locate $ Name $1 }

Chan :: {Chan}
    : termName                                  {% locate $ Chan $1 }

TypeName :: {TypeName}
    : typeName                                  {% locate $ TypeName $1 }

TypeVar :: {TypeVar}
    : typeName                                  {% locate $ TypeVar $1 }

{}
