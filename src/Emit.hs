{-# LANGUAGE OverloadedStrings #-}

module Emit where

import Codegen
import qualified Parser as P

import Debug.Trace (trace)

import Control.Monad.Except
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Char8 (pack)
import Data.ByteString.Short (toShort)

import LLVM.Module
import LLVM.Context

import qualified LLVM.AST as AST
import qualified LLVM.AST.Type as T
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.FloatingPointPredicate as FP

annotateArgTypes :: [String] -> [(AST.Type, AST.Name)]
annotateArgTypes = map (\n -> (double, AST.Name $ toShort $ pack n))

codegen :: P.Expression -> LLVM ()
codegen (P.FunctionDefinition name args body) = do
    -- FIXME: wrong, should be a pointer type??
    define double name fnArgs blks
    where
        fnArgs = annotateArgTypes args
        blks = createBlocks $ execCodegen $ do
            entry <- addBlock entryBlockName
            setBlock entry
            forM args $ \a -> do
                var <- alloca double
                store var (local (AST.Name $ toShort $ pack a))
                assign a var
            let body' = case body of
                    P.Block exprs -> last exprs
                    e -> e
                in cgen body' >>= ret

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
cgen e | trace ("cgen: " ++ show e) False = undefined
cgen (P.FloatConstant n) = return $ constOp $ C.Float (F.Double n)
cgen (P.IntegerConstant n) = return $ constOp $ C.Float (F.Double $ fromIntegral n)
cgen (P.Identifier i) = getvar i >>= load
cgen (P.FunctionCall fn args) = do
    largs <- mapM cgen args
    call (externf (AST.Name $ toShort $ pack fn)) largs

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
