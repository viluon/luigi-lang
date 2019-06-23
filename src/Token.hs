
module Token where

import SourcePos

data TokenClass = TIdentifier String
                | TComment String
                | TNumber Double
                | TEscape Char
                | TEof
                | TPlus
                | TMinus
                | TSlash
                | TTilde
                | TColon
                | TEquals
                | TAsterisk
                | TLessThan
                | TWildcard
                | TSemicolon
                | TCurlyOpen
                | TParenOpen
                | TSquareOpen
                | TCurlyClose
                | TParenClose
                | TSquareClose
                | TPercentSign
                | TGreaterThan
                | TLessOrEqual
                | TQuestionMark
                | TGreaterOrEqual
                | TExclamationMark
              deriving (Eq, Show)

data Token = Token !TokenClass !SourcePos !SourcePos
          deriving Show
