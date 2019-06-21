{
module Lexer where

import Token
import SourcePos
}

%wrapper "monad"

$digit = [0-9]
$alpha = [a-zA-Z]
@identifier = ($alpha($digit|$alpha|_)*|(($alpha|_)($digit|$alpha|_)+))'*

tokens :-
            $white+                 ;
            "--".*                  { simply Comment                    }
            \+                      { symbol Plus                       }
            \-                      { symbol Minus                      }
            \/                      { symbol Slash                      }
            \~                      { symbol Tilde                      }
            \,                      { symbol Colon                      }
            =                       { symbol Equals                     }
            \*                      { symbol Asterisk                   }
            \<                      { symbol LessThan                   }
            _                       { symbol Wildcard                   }
            \;                      { symbol Semicolon                  }
            \{                      { symbol CurlyOpen                  }
            \(                      { symbol ParenOpen                  }
            \[                      { symbol SquareOpen                 }
            \}                      { symbol CurlyClose                 }
            \)                      { symbol ParenClose                 }
            \]                      { symbol SquareClose                }
            \>                      { symbol GreaterThan                }
            \?                      { symbol QuestionMark               }
            \!                      { symbol ExclamationMark            }
            @identifier             { simply Identifier                 }
            \"                      { begin string                      }
<string>    \\.                     { simply (\s -> Escape (last s))    }
<string>    \"                      { begin 0                           }

{
alexEOF :: Alex Token
-- FIXME: we'd like the actual EOF position here
alexEOF = return (Token Eof (SourcePos "" 0 0) (SourcePos "" 0 0))

simply :: (String -> TokenClass) -> (AlexPosn, Char, [Byte], String) -> Int -> Alex Token
simply t ((AlexPn _ row col), _, _, s) len = let pos = SourcePos "" row col
                                             in let epos = pos `offsetBy` len
                                                in return (Token (t s) pos epos)

symbol :: TokenClass -> (AlexPosn, Char, [Byte], String) -> Int -> Alex Token
symbol t ((AlexPn _ row col), _, _, _) len = let pos = SourcePos "" row col
                                             in let epos = pos `offsetBy` len
                                                in return (Token t pos epos)

-- TODO
beginString :: Alex Token
beginString = undefined
-- beginString = do
--     mapState $ \s -> s {}
--     setStartCode string
--     lexerScan

lexwrap :: (Token -> Alex a) -> Alex a
lexwrap cont = do
    token <- alexMonadScan
    cont token
}
