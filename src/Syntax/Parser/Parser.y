{
{-# LANGUAGE OverloadedStrings                  #-}

module Syntax.Parser.Parser where

import Syntax.Parser.Lexer
import Syntax.Parser.Type
import Syntax.Concrete
import Data.Loc
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

Program :: {Program Loc}
    : Declarations                          {% locate $ Program (reverse $1) }

-- left recursive
Declarations :: {[Declaration Loc]}
    : Declaration                           { [$1] }
    | Declarations Declaration              { $2:$1 }

Declaration :: {Declaration Loc}
    : TermName ':' Type                         {% locate $ TypeSig $1 $3 }
    | TermName '=' Process                      {% locate $ TermDefn $1 $3 }

Process :: {Process Loc}
    : TermName '<->' TermName                   {% locate $ Link $1 $3  }
    | 'nu' TermName '.' '(' Process '|' Process ')'             {% locate $ Compose $2 $5 $7 }
    | TermName '[' TermName ']' '.' '(' Process '|' Process ')' {% locate $ Output $1 $3 $7 $9 }
    | TermName '(' TermName ')' '.' Process                     {% locate $ Input $1 $3 $6 }
    | TermName '[inl]' '.' Process              {% locate $ SelectL $1 $4 }
    | TermName '[inr]' '.' Process              {% locate $ SelectR $1 $4 }
    | TermName '.' 'case' '(' Process ',' Process ')'           {% locate $ Choice $1 $5 $7 }
    | '!' TermName '(' TermName ')' '.' Process {% locate $ Accept $2 $4 $7 }
    | '?' TermName '[' TermName ']' '.' Process {% locate $ Request $2 $4 $7 }
    | TermName '[]' '.' 'end'                   {% locate $ EmptyOutput $1 }
    | TermName '()' '.' Process                 {% locate $ EmptyInput $1 $4 }
    | TermName '.' 'case()'                     {% locate $ EmptyChoice $1 }

Type :: {Type Loc}
    : Type1                                     { $1 }
    | 'exists' TypeName Type1                   {% locate $ Exists $2 $3 }
    | 'forall' TypeName Type1                   {% locate $ Forall $2 $3 }

-- right associative
Type1 :: {Type Loc}
    : Type2                                     { $1 }
    | Type2 '*' Type1                           {% locate $ Times $1 $3 }
    | Type2 '%' Type1                           {% locate $ Par $1 $3 }

-- right associative
Type2 :: {Type Loc}
    : Type3                                     { $1 }
    | Type3 '+' Type2                           {% locate $ Plus $1 $3 }
    | Type3 '&' Type2                           {% locate $ With $1 $3 }

Type3 :: {Type Loc}
    : Type4                                     { $1 }
    | '!' Type3                                 {% locate $ Acc $2 }
    | '?' Type3                                 {% locate $ Req $2 }

Type4 :: {Type Loc}
    : '(' Type ')'                              { $2 }
    |  '^' Type4                                {% locate $ Dual $2  }
    | '1'                                       {% locate $ One }
    | 'Bot'                                     {% locate $ Bot }
    | '0'                                       {% locate $ Zero }
    | 'Top'                                     {% locate $ Top }


TermName :: {TermName Loc}
      : termName                                {% locate $ TermName $1 }

TypeName :: {TypeName Loc}
      : typeName                                {% locate $ TypeName $1 }

{}
