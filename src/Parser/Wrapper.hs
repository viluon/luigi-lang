
module Parser.Wrapper
    ( Parser
    , ParserState
    , ParseResult
    , ParseError( .. )
    , AlexInput( .. )
    , getStartCode
    , alexGetByte
    , getState
    , getInput
    , getPos
    , failWith
    ) where

import Data.Int (Int64)
import Data.Char (ord)
import Data.Word (Word8)

import SourcePos
import Token

import Control.Monad
import qualified Control.Monad.Fail as MonadFail

data ParserState = ParserState {
      statePos  :: !SourcePos
    , stateText :: String
    , statePrev :: !Char
    , stateIdx  :: !Int64
    , stateMode :: !Int
    , stateErrors  :: [ParseError]
    , stateErrored :: Bool
} deriving Show

data ParseResult a = ParseOK ParserState a
                   | ParseFail [ParseError]
                   deriving Show

data ParseError = UnexpectedToken Token
                | UnexpectedCharacter SourcePos Char
                | Failure SourcePos String
                deriving Show

newtype Parser a = Parser { doParse :: ParserState -> ParseResult a }

instance Functor Parser where
    fmap = liftM

instance Applicative Parser where
    pure a = a `seq` (Parser $ flip ParseOK a)
    (<*>) = ap

instance Monad Parser where
    (Parser m) >>= k = Parser $ \s -> case m s of
        ParseOK s' a -> doParse (k a) s'
        ParseFail e -> ParseFail e
    fail = MonadFail.fail

instance MonadFail.MonadFail Parser where
    fail msg = Parser $ \s -> ParseFail (Failure (statePos s) msg:(stateErrors s))

data AlexInput = LexerInput { lexiPos  :: !SourcePos
                            , lexiText :: !String
                            , lexiPrev :: !Char
                            , lexiIdx  :: !Int64 }

getState :: Parser ParserState
getState = Parser (\s -> ParseOK s s)

getPos :: Parser SourcePos
getPos = Parser $ \s -> ParseOK s (statePos s)

getStartCode :: Parser Int
getStartCode = Parser $ \s -> ParseOK s (stateMode s)

getInput :: Parser AlexInput
getInput = Parser (\s -> ParseOK s LexerInput { lexiPos  = statePos  s
                                              , lexiText = stateText s
                                              , lexiPrev = statePrev s
                                              , lexiIdx  = stateIdx  s
                                              } )

failWith :: ParseError -> Parser a
failWith err = Parser $ \s -> ParseFail (err:stateErrors s)

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte LexerInput { lexiPos = p, lexiText = t, lexiIdx = n } =
    case t of
        [] -> Nothing
        _ -> let (c:t') = t
             in let b = fromIntegral (ord c)
                in Just (b, LexerInput { lexiPos = advanceLexer p c
                             , lexiText = t'
                             , lexiPrev = c
                             , lexiIdx = n + 1 })

advanceLexer :: SourcePos -> Char -> SourcePos
advanceLexer (SourcePos file row _) '\n' = SourcePos file (row + 1) 1
advanceLexer pos                    _    = pos `offsetBy` 1
