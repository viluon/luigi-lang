
module Parser.Wrapper
    ( Parser( .. )
    , ParserState
    , ParseResult( .. )
    , ParseError( .. )
    , AlexInput( .. )
    , getStartCode
    , setStartCode
    , alexGetByte
    , getState
    , getInput
    , setInput
    , getPos
    , failWith
    , runParser
    , emptyParser
    ) where

import Data.Int (Int64)
import Data.Char (ord)
import Data.Word (Word8)

import SourcePos
import Token

import Debug.Trace (trace)

import Control.Monad
import qualified Control.Monad.Fail as MonadFail

data ParserState = ParserState {
      statePos       :: !SourcePos
    , stateText      :: String
    , statePrev      :: !Char
    , stateIdx       :: !Int64
    , stateStartCode :: !Int
    , stateErrors    :: [ParseError]
    , stateErrored   :: Bool
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
                            deriving Show

getState :: Parser ParserState
getState = Parser (\s -> ParseOK s s)

getPos :: Parser SourcePos
getPos = Parser $ \s -> ParseOK s (statePos s)

getStartCode :: Parser Int
getStartCode = Parser $ \s -> ParseOK s (stateStartCode s)

getInput :: Parser AlexInput
getInput = Parser (\s -> ParseOK s LexerInput { lexiPos  = statePos  s
                                              , lexiText = stateText s
                                              , lexiPrev = statePrev s
                                              , lexiIdx  = stateIdx  s
                                              } )

setStartCode :: Int -> Parser ()
setStartCode n = Parser $ \s -> ParseOK (s { stateStartCode = n }) ()

setInput :: AlexInput -> Parser ()
setInput input = Parser (\s -> ParseOK (s {
          statePos  = lexiPos  input
        , stateText = lexiText input
        , statePrev = lexiPrev input
        , stateIdx  = lexiIdx  input
    })())

failWith :: ParseError -> Parser a
failWith err = Parser $ \s -> ParseFail (err:stateErrors s)

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte LexerInput { lexiPos = p, lexiText = t, lexiIdx = n } =
    case t of
        [] -> Nothing
        (c:t') -> let b = fromIntegral (ord c)
                  in  Just (b, LexerInput { lexiPos = advanceLexer p c
                              , lexiText = t'
                              , lexiPrev = c
                              , lexiIdx = n + 1 })

advanceLexer :: SourcePos -> Char -> SourcePos
advanceLexer (SourcePos file row _) '\n' = SourcePos file (row + 1) 1
advanceLexer pos                    _    = pos `offsetBy` 1

runParser :: String -> String -> Parser a -> (Maybe a, [ParseError], Maybe ParserState)
runParser path input parser =
    let initialState = ParserState {
          statePos       = SourcePos path 1 1
        , stateText      = input
        , statePrev      = '\n'
        , stateIdx       = 0
        , stateStartCode = 0
        , stateErrors    = []
        , stateErrored   = False
    } in case doParse parser initialState of
        ParseFail errors -> (Nothing, reverse errors, Nothing)
        ParseOK state result | stateErrored state -> (Nothing,     reverse $ stateErrors state, Just state)
                             | otherwise          -> (Just result, reverse $ stateErrors state, Just state)

emptyParser :: Parser (IO ())
emptyParser = (Parser $ \s -> ParseOK s (putStrLn (show $ statePos s)))
