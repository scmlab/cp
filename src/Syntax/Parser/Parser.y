{
{-# LANGUAGE OverloadedStrings                  #-}

module Syntax.Parser.Parser where

import Syntax.Parser.Lexer
import Syntax.Parser.Type
import Syntax.Concrete
import Syntax.Base
import Data.Loc hiding (Pos)
import Prelude hiding (GT, LT, EQ)

import Data.Text (Text)

}

%name programParser Program
%tokentype { Token }
%error { syntaticalError }

%monad { Parser }
%lexer { scan } { TokenEOF }

%token
        termName        { TokenTermName $$          }
        typeName        { TokenTypeName $$          }
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

Program :: {Program Loc}
    : {- empty -}                           {% locate $ Program [] }
    | Declarations                          {% locate $ Program (reverse $1) }

-- left recursive
Declarations :: {[Declaration Loc]}
    : Declaration                           { [$1] }
    | Declarations Declaration              { $2:$1 }

Declaration :: {Declaration Loc}
    : s TermName ':' Type                         {% locate' $1 $ TypeSig $2 $4 }
    | s TermName '=' Process                      {% locate' $1 $ TermDefn $2 $4 }

Process :: {Process Loc}
    : s TermName '<->' TermName                   {% locate' $1 $ Link $2 $4  }
    | s 'nu' TermName ':' Type '.' '(' Process '|' Process ')'    {% locate' $1 $ Compose $3 $5 $8 $10 }
    | s TermName '[' TermName ']' '.' '(' Process '|' Process ')' {% locate' $1 $ Output $2 $4 $8 $10 }
    | s TermName '(' TermName ')' '.' Process                     {% locate' $1 $ Input $2 $4 $7 }
    | s TermName '[inl]' '.' Process              {% locate' $1 $ SelectL $2 $5 }
    | s TermName '[inr]' '.' Process              {% locate' $1 $ SelectR $2 $5 }
    | s TermName '.' 'case' '(' Process ',' Process ')'           {% locate' $1 $ Choice $2 $6 $8 }
    | s '!' TermName '(' TermName ')' '.' Process {% locate' $1 $ Accept $3 $5 $8 }
    | s '?' TermName '[' TermName ']' '.' Process {% locate' $1 $ Request $3 $5 $8 }
    | s TermName '[' Type ']' '.' Process         {% locate' $1 $ OutputT $2 $4 $7 }
    | s TermName '(' TypeVar ')' '.' Process     {% locate' $1 $ InputT $2 $4 $7 }
    | s TermName '[]' '.' 'end'                   {% locate' $1 $ EmptyOutput $2 }
    | s TermName '()' '.' Process                 {% locate' $1 $ EmptyInput $2 $5 }
    | s TermName '.' 'case()'                     {% locate' $1 $ EmptyChoice $2 }

Type :: {Type Loc}
    : s Type1                                   { $2 }
    | s 'exists' TypeVar Type1                     {% locate' $1 $ Exists $3 $4 }
    | s 'forall' TypeVar Type1                 {% locate' $1 $ Forall $3 $4 }

-- right associative
Type1 :: {Type Loc}
    : s Type2                                   { $2 }
    | s Type2 '*' Type1                         {% locate' $1 $ Times $2 $4 }
    | s Type2 '%' Type1                         {% locate' $1 $ Par $2 $4 }

-- right associative
Type2 :: {Type Loc}
    : s Type3                                   { $2 }
    | s Type3 '+' Type2                         {% locate' $1 $ Plus $2 $4 }
    | s Type3 '&' Type2                         {% locate' $1 $ With $2 $4 }

Type3 :: {Type Loc}
    : s Type4                                   { $2 }
    | s '!' Type3                               {% locate' $1 $ Acc $3 }
    | s '?' Type3                               {% locate' $1 $ Req $3 }

Type4 :: {Type Loc}
    : '(' Type ')'                              { $2 }
    | s '^' Type4                               {% locate' $1 $ Dual $3  }
    | '1'                                       {% locate $ One }
    | 'Bot'                                     {% locate $ Bot }
    | '0'                                       {% locate $ Zero }
    | 'Top'                                     {% locate $ Top }

TermName :: {TermName Loc}
    : termName                                  {% locate $ TermName $1 }

TypeName :: {TypeName Loc}
    : typeName                                  {% locate $ TypeName $1 }

TypeVar :: {TypeVar}
    : typeName                                  {% return $ Named $1 }

{}
