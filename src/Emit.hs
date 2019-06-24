{-# LANGUAGE OverloadedStrings #-}

module Emit where

import Codegen
import qualified Parser as P

import Control.Monad.Except
import qualified Data.ByteString as BS

import LLVM.Module
import LLVM.Context

import qualified LLVM.AST as AST
import qualified LLVM.AST.Type as T
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.FloatingPointPredicate as FP

codegen :: P.Expression -> LLVM ()
codegen exp = do
    define double "main" [] blks
    where
        blks = createBlocks $ execCodegen $ do
            entry <- addBlock entryBlockName
            setBlock entry
            cgen exp >>= ret

constOp :: C.Constant -> AST.Operand
constOp = AST.ConstantOperand

cgen :: P.Expression -> Codegen AST.Operand
cgen (P.FloatConstant n) = return $ constOp $ C.Float (F.Double n)
cgen (P.IntegerConstant n) = return $ constOp $ C.Float (F.Double $ fromIntegral n)


liftError :: ExceptT String IO a -> IO a
liftError = runExceptT >=> either fail return

codegenWrapper :: AST.Module -> [P.Expression] -> IO AST.Module
codegenWrapper mod functions = withContext $ \context ->
    withModuleFromAST context newAST $ \m -> do
        llstr <- moduleLLVMAssembly m
        BS.putStrLn llstr
        return newAST
    where
        modn    = mapM codegen functions
        newAST  = runLLVM mod modn
