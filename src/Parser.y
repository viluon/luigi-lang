
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
    '='             { Token TEquals _ _                         }
    '<'             { Token TLessThan _ _                       }
    '*'             { Token TAsterisk _ _                       }
    ';'             { Token TSemicolon _ _                      }
    '{'             { Token TCurlyOpen _ _                      }
    '('             { Token TParenOpen _ _                      }
    '['             { Token TSquareOpen _ _                     }
    '}'             { Token TCurlyClose _ _                     }
    ')'             { Token TParenClose _ _                     }
    ']'             { Token TSquareClose _ _                    }
    '%'             { Token TPercentSign _ _                    }
    '>'             { Token TGreaterThan _ _                    }
    '<='            { Token TLessOrEqual _ _                    }
    '>='            { Token TGreaterOrEqual _ _                 }
    ident           { Token (TIdentifier $$) _ _                }
    int             { Token (TInteger $$) _ _                   }
    float           { Token (TFloat $$) _ _                     }

%%

Expr         :: { Expression }
             :  ident                    { Identifier $1                            }
             |  int                      { IntegerConstant $1                       }
             |  float                    { FloatConstant $1                         }
             |  Call                     { $1                                       }
             |  Binding                  { $1                                       }
             |  ListValue                { $1                                       }
             |  Expr '+' Expr            { ArithmeticOperation Plus           $1 $3 }
             |  Expr '-' Expr            { ArithmeticOperation Minus          $1 $3 }
             |  Expr '/' Expr            { ArithmeticOperation Div            $1 $3 }
             |  Expr '%' Expr            { ArithmeticOperation Mod            $1 $3 }
             |  Expr '*' Expr            { ArithmeticOperation Times          $1 $3 }
             |  Expr '<' Expr            { ComparisonOperation LessThan       $1 $3 }
             |  Expr '>' Expr            { ComparisonOperation GreaterThan    $1 $3 }
             |  Expr '<=' Expr           { ComparisonOperation LessOrEqual    $1 $3 }
             |  Expr '>=' Expr           { ComparisonOperation GreaterOrEqual $1 $3 }
             |  '(' Expr ')'             { $2                                       }
             |  '-' Expr %prec NEG       { ArithmeticNegate $2                      }
             |  '{' ExprBlock '}'        { Block $2                                 }

ExprListRev  :: { [Expression] }
             :  Expr                     { [$1]          }
             |  ExprListRev ',' Expr     { $3 : $1       }

ExprList     :: { [Expression] }
             :  ExprListRev              { reverse $1    }

ExprBlockRev :: { [Expression] }
             :  Expr                     { [$1]          }
             |  ExprBlockRev ';' Expr    { $3 : $1       }

ExprBlock    :: { [Expression] }
             :  ExprBlockRev             { reverse $1    }

ListValue    :: { Expression }
             :  '[' ']'                  { ListValue []  }
             |  '[' ExprList ']'         { ListValue $2  }

Binding      :: { Expression }
             :  Expr '=' '>' ident       { ImmutableBinding $1 $4 }
             |  Expr '~' '>' ident       {   MutableBinding $1 $4 }

Call         :: { Expression }
             :  ident '(' ')'            { FunctionCall $1 []     }
             |  ident '(' ExprList ')'   { FunctionCall $1 $3     }

{

data Expression = Identifier String
                | IntegerConstant Int
                | FloatConstant Double
                | Block [Expression]
                | ListValue [Expression]
                | FunctionCall String [Expression]
                | MutableBinding Expression String
                | ImmutableBinding Expression String
                | ArithmeticNegate Expression
                | ArithmeticOperation ArithmeticOperator Expression Expression
                | ComparisonOperation ComparisonOperator Expression Expression
                deriving (Show, Eq)

data ArithmeticOperator = Mod
                        | Div
                        | Plus
                        | Minus
                        | Times
                        deriving (Show, Eq)

data ComparisonOperator = LessThan
                        | GreaterThan
                        | LessOrEqual
                        | GreaterOrEqual
                        deriving (Show, Eq)

parseError :: (Token, [String]) -> Parser a
parseError (t, _) = failWith (UnexpectedToken t)

lexer :: (Token -> Parser a) -> Parser a
lexer = (lexerScan >>=)

}
