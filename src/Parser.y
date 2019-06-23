
{
module Parser where

import Lexer
import Token
import SourcePos
import Parser.Wrapper
}

%name parseBlock ExprBlock
%tokentype { Token }
%error { parseError }
%monad { Parser } { (>>=) } { return }
%lexer { lexer } { Token TEof _ _ }
%errorhandlertype explist

%token
    '+'             { Token TPlus _ _                           }
    '-'             { Token TMinus _ _                          }
    '/'             { Token TSlash _ _                          }
    '~'             { Token TTilde _ _                          }
    ','             { Token TColon _ _                          }
    ';'             { Token TSemicolon _ _                      }
    ident           { Token (TIdentifier $$) _ _                }

%%

Expr        :: { Expression }
            : ident                 { Identifier $1 }

ExprList    :: { [Expression] }
            : Expr                  { [$1]          }
            | ExprList ',' Expr     { $3 : $1       }

ExprBlock   :: { [Expression] }
            : Expr                  { [$1]          }
            | ExprBlock ';' Expr    { $3 : $1       }

{

data Expression = Identifier String
                deriving (Show, Eq)

parseError :: (Token, [String]) -> Parser a
parseError (t, _) = failWith (UnexpectedToken t)

lexer :: (Token -> Parser a) -> Parser a
lexer = (lexerScan >>=)

}
