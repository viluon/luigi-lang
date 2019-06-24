
module Token where

import SourcePos

data TokenClass = TIdentifier String
                | TComment String
                | TFloat Double
                | TInteger Int
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
                | TNotEquals
                | TSemicolon
                | TCurlyOpen
                | TParenOpen
                | TEqualsSign
                | TSquareOpen
                | TCurlyClose
                | TParenClose
                | TSquareClose
                | TPercentSign
                | TGreaterThan
                | TLessOrEqual
                | TBindingArrow
                | TQuestionMark
                | TGreaterOrEqual
                | TExclamationMark
                | TWavyBindingArrow
              deriving (Eq, Show)

data Token = Token !TokenClass !SourcePos !SourcePos
          deriving Show
