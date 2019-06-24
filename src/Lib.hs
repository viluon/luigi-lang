module Lib
    ( main
    ) where

import qualified LLVM.AST as AST

import Debug.Trace (trace)

import Parser
import Parser.Wrapper
import Codegen
import Emit

initModule :: AST.Module
initModule = emptyModule "Luigi module"

process :: AST.Module -> String -> IO (Maybe AST.Module)
process mod source = do
    let res = runParser "stdin" source parseBlock
     in case res of
        _ | trace ("parsed " ++ (show res)) False -> undefined
        (Nothing,  errs, _) -> print (show errs) >> return Nothing
        (Just ast, _,    _) -> do
            ir <- codegenWrapper mod ast
            return $ Just ir

main :: IO ()
main = do
    source <- getContents
    process initModule source
    return ()
