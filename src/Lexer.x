{
module Lexer where

import Token
import SourcePos
import Parser.Wrapper
import Debug.Trace(trace)
}

$digit = [0-9]
$alpha = [a-zA-Z]
@identifier = ($alpha($digit|$alpha|_)*|(($alpha|_)($digit|$alpha|_)+))'*

tokens :-
            $white+                 ;
            "--".*                  { trivial TComment                   }
            \+                      { symbol TPlus                       }
            \-                      { symbol TMinus                      }
            \/                      { symbol TSlash                      }
            \~                      { symbol TTilde                      }
            \,                      { symbol TColon                      }
            =                       { symbol TEquals                     }
            \*                      { symbol TAsterisk                   }
            \<                      { symbol TLessThan                   }
            _                       { symbol TWildcard                   }
            \;                      { symbol TSemicolon                  }
            \{                      { symbol TCurlyOpen                  }
            \(                      { symbol TParenOpen                  }
            \[                      { symbol TSquareOpen                 }
            \}                      { symbol TCurlyClose                 }
            \)                      { symbol TParenClose                 }
            \]                      { symbol TSquareClose                }
            \%                      { symbol TPercentSign                }
            \>                      { symbol TGreaterThan                }
            \?                      { symbol TQuestionMark               }
            \<=                     { symbol TLessOrEqual                }
            \>=                     { symbol TGreaterOrEqual             }
            \!                      { symbol TExclamationMark            }
            @identifier             { simply TIdentifier                 }
<string>    \"                      { endString                          }

{
alexEOF :: Parser Token
-- FIXME: we'd like the actual EOF position here
alexEOF = return (Token TEof (SourcePos "" 0 0) (SourcePos "" 0 0))

simply :: (String -> TokenClass) -> AlexInput -> Int -> Parser Token
simply t (LexerInput (SourcePos _ row col) s _ _) len = let pos = SourcePos "" row col
                                                           in let epos = pos `offsetBy` len
                                                              in return (Token (t $ take len s) pos epos)

symbol :: TokenClass -> AlexInput -> Int -> Parser Token
symbol t (LexerInput (SourcePos _ row col) _ _ _) len = let pos = SourcePos "" row col
                                                in let epos = pos `offsetBy` len
                                                   in return (Token t pos epos)


-- TODO: switch handling of trivials on/off
trivial :: (String -> TokenClass) -> AlexInput -> Int -> Parser Token
trivial _ _ _ = lexerScan

-- TODO
beginString :: Parser Token
beginString = do
    setStartCode string
    lexerScan

endString :: AlexInput -> Int -> Parser Token
endString _ _ = do
    setStartCode 0
    lexerScan

lexerScan :: Parser Token
lexerScan = do
    input     <- getInput
    startCode <- getStartCode
    case alexScan input startCode of
        AlexEOF -> do
            code <- getStartCode
            case code of
                0 -> (\p -> Token TEof p p) <$> getPos
                _ -> failWith (Failure (lexiPos input) "unexpected <eof>")
        AlexError (LexerInput pos str _ _) ->
             failWith (UnexpectedCharacter pos (head str))
        AlexSkip input' _ -> do
            trace "AlexSkip" setInput input'
            lexerScan
        AlexToken input' _ action -> do
            setInput input'
            -- action :: AlexInput -> Int -> Parser Token
            action (trace ("AlexToken: " ++ (show input)) input) (fromIntegral $ lexiIdx input' - lexiIdx input)
}
