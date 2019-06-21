
module Parser.Wrapper
    ( Parser
    , ParserState
    , ParseResult
    ) where

import Data.Int (Int64)

import SourcePos
import Token

import Control.Monad
import qualified Control.Monad.Fail as MonadFail

data ParserState = ParserState {
      sPos  :: !SourcePos
    , sText :: String
    , sPrev :: !Char
    , sIdx  :: !Int64
    , sMode :: !Int
    , sErrors  :: [ParseError]
    , sErrored :: Bool
} deriving Show

data ParseResult a = ParseOK ParserState a
                   | ParseFail [ParseError]
                   deriving Show

data ParseError = UnexpectedToken Token
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
    fail msg = Parser $ \s -> ParseFail (Failure (sPos s) msg:(sErrors s))
