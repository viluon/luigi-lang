
{
module Parser where

import Lexer
import Token
import SourcePos
import Parser.Wrapper
}

%name parseExpression Expr
%tokentype { Token }
%error { parseError }
%monad { Parser } { (>>=) } { return }
%lexer { lexer } { Token Eof _ _ }
%errorhandlertype explist

%token
    '+'             { Token Plus _ _                           }
    '-'             { Token Minus _ _                          }
    '/'             { Token Slash _ _                          }
    '~'             { Token Tilde _ _                          }
    ','             { Token Colon _ _                          }
    ';'             { Token Semicolon _ _                      }
    ident           { Token (Identifier $$) _ _                }

%%

Expr        :: { Expression }
            : ident                 { Expression $1 }

{
--ExprList    :: { [Expression] }
--            : Expr                  { [$1]          }
--            | ExprList ',' Expr     { $2 : $1       }
--
--ExprBlock   :: { [Expression] }
--            : Expr                  { [$1]          }
--            | ExprBlock ';' Expr    { $2 : $1       }

data Expression = Expression String
                deriving (Show, Eq)

parseError :: (Token, [String]) -> Parser a
parseError _ = error "Parse error"

lexer :: (Token -> Parser a) -> Parser a
lexer = (lexerScan >>=)

}
