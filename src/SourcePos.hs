
module SourcePos where

data SourcePos = SourcePos String Int Int

instance Show SourcePos where
    show (SourcePos file row column) = "<" ++ file ++ ":" ++ show row ++ ":" ++ show column ++ ">"

offsetBy :: SourcePos -> Int -> SourcePos
(SourcePos file row col) `offsetBy` len = SourcePos file row (col + len)
