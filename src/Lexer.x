{
module Lexer where

import Token
import SourcePos
import Parser.Wrapper
}

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

{
alexEOF :: Parser Token
-- FIXME: we'd like the actual EOF position here
alexEOF = return (Token Eof (SourcePos "" 0 0) (SourcePos "" 0 0))

simply :: (String -> TokenClass) -> AlexInput -> Int -> Parser Token
simply t (LexerInput (SourcePos _ row col) s _ _) len = let pos = SourcePos "" row col
                                                           in let epos = pos `offsetBy` len
                                                              in return (Token (t s) pos epos)

symbol :: TokenClass -> AlexInput -> Int -> Parser Token
symbol t (LexerInput (SourcePos _ row col) _ _ _) len = let pos = SourcePos "" row col
                                                in let epos = pos `offsetBy` len
                                                   in return (Token t pos epos)

-- TODO
beginString :: Parser Token
beginString = undefined
-- beginString = do
--     mapState $ \s -> s {}
--     setStartCode string
--     lexerScan

--alex2parser :: Alex a -> Parser a
--alex2parser alex = do
--    case alex of
--        Alex (Token c s e) -> Parser (Token c s e)

--lexwrap :: (Token -> Alex a) -> Alex a
--lexwrap cont = do
--    token <- alexMonadScan
--    cont token


lexerScan :: Parser Token
lexerScan = do
    input     <- getInput
    startCode <- getStartCode
    case alexScan input startCode of
        AlexEOF -> do
            code <- getStartCode
            case code of
                0 -> (\p -> Token Eof p p) <$> getPos
                _ -> failWith (Failure (lexiPos input) "unexpected <eof>")
        AlexError (LexerInput pos str _ _) ->
             failWith (UnexpectedCharacter pos (head str))
        _ -> failWith (Failure (SourcePos "?" 0 0) "what")
}
