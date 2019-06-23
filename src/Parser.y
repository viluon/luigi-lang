
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

%nonassoc '<' '>' '<=' '>='
%left '+' '-'
%left '*' '/' '%'
%left NEG

%token
    '+'             { Token TPlus _ _                           }
    '-'             { Token TMinus _ _                          }
    '/'             { Token TSlash _ _                          }
    '~'             { Token TTilde _ _                          }
    ','             { Token TColon _ _                          }
    '<'             { Token TLessThan _ _                       }
    '*'             { Token TAsterisk _ _                       }
    ';'             { Token TSemicolon _ _                      }
    '('             { Token TParenOpen _ _                      }
    ')'             { Token TParenClose _ _                     }
    '['             { Token TSquareOpen _ _                     }
    ']'             { Token TSquareClose _ _                    }
    '%'             { Token TPercentSign _ _                    }
    '>'             { Token TGreaterThan _ _                    }
    '<='            { Token TLessOrEqual _ _                    }
    '>='            { Token TGreaterOrEqual _ _                 }
    ident           { Token (TIdentifier $$) _ _                }

%%

Expr         :: { Expression }
             : ident                    { Identifier $1          }
             | ListValue                { $1                     }
             | Expr '+' Expr            { OpPlus           $1 $3 }
             | Expr '-' Expr            { OpMinus          $1 $3 }
             | Expr '/' Expr            { OpDiv            $1 $3 }
             | Expr '%' Expr            { OpMod            $1 $3 }
             | Expr '*' Expr            { OpTimes          $1 $3 }
             | Expr '<' Expr            { OpLessThan       $1 $3 }
             | Expr '>' Expr            { OpGreaterThan    $1 $3 }
             | Expr '<=' Expr           { OpLessOrEqual    $1 $3 }
             | Expr '>=' Expr           { OpGreaterOrEqual $1 $3 }
             | '(' Expr ')'             { $2                     }
             | '-' Expr %prec NEG       { OpNegate $2            }

ExprListRev  :: { [Expression] }
             : Expr                     { [$1]          }
             | ExprListRev ',' Expr     { $3 : $1       }

ExprList     :: { [Expression] }
             :  ExprListRev             { reverse $1    }

ExprBlockRev :: { [Expression] }
             : Expr                     { [$1]          }
             | ExprBlockRev ';' Expr    { $3 : $1       }

ExprBlock    :: { [Expression] }
             : ExprBlockRev             { reverse $1    }

ListValue    :: { Expression }
             : '[' ']'                  { ListValue []  }
             | '[' ExprList ']'         { ListValue $2  }

{

data Expression = Identifier String
                | IntegerConstant Int
                | DoubleConstant Double
                | ListValue [Expression]
                | OpMod Expression Expression
                | OpDiv Expression Expression
                | OpPlus Expression Expression
                | OpMinus Expression Expression
                | OpTimes Expression Expression
                | OpNegate Expression
                | OpLessThan Expression Expression
                | OpGreaterThan Expression Expression
                | OpLessOrEqual Expression Expression
                | OpGreaterOrEqual Expression Expression
                deriving (Show, Eq)

parseError :: (Token, [String]) -> Parser a
parseError (t, _) = failWith (UnexpectedToken t)

lexer :: (Token -> Parser a) -> Parser a
lexer = (lexerScan >>=)

}
