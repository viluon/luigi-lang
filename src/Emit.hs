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
codegen e | trace ("codegen: " ++ show e) False = undefined
codegen (P.FunctionDefinition name args body) = do
    -- FIXME: wrong, should be a pointer type??
    define double name fnArgs blks
    where
        fnArgs = annotateArgTypes args
        blks = createBlocks $ execCodegen $ do
            entry <- addBlock entryBlockName
            setBlock entry
            forM args $ \argName -> do
                var <- alloca double
                store var (local (AST.Name $ toShort $ pack argName))
                assign argName var
            let body' = case body of
                    _ | trace "body' stuff" False -> undefined
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
    call (externf (length largs) (AST.Name $ toShort $ pack fn)) largs

cgen (P.ArithmeticOperation op l r) = do
    left <- cgen l
    right <- cgen r
    fn left right
        where fn = case op of
                    P.Div -> fdiv
                    P.Plus -> fadd
                    P.Times -> fmul
                    P.Minus -> fsub

cgen (P.ComparisonOperation op l r) = do
    left <- cgen l
    right <- cgen r
    fn left right
        where fn = case op of
                    P.Equals -> cmp FP.OEQ
                    P.NotEquals -> cmp FP.ONE
                    P.LessThan -> cmp FP.ULT
                    P.GreaterThan -> cmp FP.UGT
                    P.LessOrEqual -> cmp FP.ULE
                    P.GreaterOrEqual -> cmp FP.UGE

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
