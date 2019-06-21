
module Token where

import SourcePos

data TokenClass = Identifier String
                | Comment String
                | Number Double
                | Escape Char
                | Eof
                | Plus
                | Minus
                | Slash
                | Tilde
                | Colon
                | Equals
                | Asterisk
                | LessThan
                | Wildcard
                | Semicolon
                | CurlyOpen
                | ParenOpen
                | SquareOpen
                | CurlyClose
                | ParenClose
                | SquareClose
                | GreaterThan
                | QuestionMark
                | ExclamationMark
              deriving (Eq, Show)

data Token = Token !TokenClass !SourcePos !SourcePos
          deriving Show
