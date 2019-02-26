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
        name            { TokenName $$          }
        '='             { TokenDefn             }
        '<->'           { TokenLink             }
        'nu'            { TokenScope            }
        '.'             { TokenSeq              }
        '|'             { TokenParComp          }
        '('             { TokenParenStart       }
        ')'             { TokenParenEnd         }
        '['             { TokenBracketStart     }
        ']'             { TokenBracketEnd       }
        '[inl]'         { TokenSelectL          }
        '[inr]'         { TokenSelectR          }
        'case'          { TokenCase             }
        ','             { TokenCaseSep          }
        '!'             { TokenAccept           }
        '?'             { TokenRequest          }
        '[]'            { TokenEmptyOutput      }
        '0'             { TokenEnd              }
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
    : Name '=' Process                      {% locate $ Declaration $1 $3 }

Process :: {Process Loc}
    : Name '<->' Name                       {% locate $ Link $1 $3  }
    | 'nu' Name '.' '(' Process '|' Process ')'         {% locate $ Par $2 $5 $7 }
    | Name '[' Name ']' '.' '(' Process '|' Process ')' {% locate $ Output $1 $3 $7 $9 }
    | Name '(' Name ')' '.' Process                     {% locate $ Input $1 $3 $6 }
    | Name '[inl]' '.' Process              {% locate $ SelectL $1 $4 }
    | Name '[inr]' '.' Process              {% locate $ SelectR $1 $4 }
    | Name '.' 'case' '(' Process ',' Process ')'       {% locate $ Choice $1 $5 $7 }
    | '!' Name '(' Name ')' '.' Process     {% locate $ Accept $2 $4 $7 }
    | '?' Name '[' Name ']' '.' Process     {% locate $ Request $2 $4 $7 }
    | Name '[]' '.' '0'                     {% locate $ EmptyOutput $1 }
    | Name '()' '.' Process                 {% locate $ EmptyInput $1 $4 }
    | Name '.' 'case()'                     {% locate $ EmptyChoice $1 }


Name :: {Name Loc}
      : name                                {% locate $ Name $1 }

{}
