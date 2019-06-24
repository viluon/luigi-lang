{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Codegen where

import qualified Data.Map as Map
import Data.ByteString.Char8 (pack)
import Data.ByteString.Short (ShortByteString, toShort)
import Data.List (sortBy)
import Data.Function (on)

import Control.Monad.State

import LLVM.AST
import LLVM.AST.Typed (typeOf)
import LLVM.AST.AddrSpace
import LLVM.AST.Type
import LLVM.AST.Global

import qualified LLVM.AST as AST
import qualified LLVM.AST.Linkage as L
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Attribute as A
import qualified LLVM.AST.CallingConvention as CC
import qualified LLVM.AST.FloatingPointPredicate as FP

double :: Type
double = FloatingPointType DoubleFP

type SymbolTable = [(String, Operand)]
type Names = Map.Map String Int

data CodegenState = CodegenState {
      currentBlock :: Name
    , blocks :: Map.Map Name BlockState
    , symTable :: SymbolTable
    , blockCount :: Int
    , count :: Word
    , names :: Names
} deriving Show

data BlockState = BlockState {
      idx :: Int
    , stack :: [Named Instruction]
    , term :: Maybe (Named Terminator)
} deriving Show

newtype Codegen a = Codegen { runCodegen :: State CodegenState a }
    deriving (Functor, Applicative, Monad, MonadState CodegenState)

newtype LLVM a = LLVM (State AST.Module a)
  deriving (Functor, Applicative, Monad, MonadState AST.Module)

runLLVM :: AST.Module -> LLVM a -> AST.Module
runLLVM mod (LLVM m) = execState m mod

emptyModule :: String -> AST.Module
emptyModule label = defaultModule { moduleName = toShort $ pack label }

addDefinition :: Definition -> LLVM ()
addDefinition d = do
    defs <- gets moduleDefinitions
    modify $ \s -> s { moduleDefinitions = defs ++ [d] }

define ::  Type -> String -> [(Type, Name)] -> [BasicBlock] -> LLVM ()
define returnType label argTypes body = addDefinition $
    GlobalDefinition $ functionDefaults {
          name        = Name $ toShort $ pack label
        , parameters  = ([Parameter tpe name [] | (tpe, name) <- argTypes], False)
        , returnType  = returnType
        , basicBlocks = body
    }

external ::  Type -> String -> [(Type, Name)] -> LLVM ()
external returnType label argTypes = addDefinition $
    GlobalDefinition $ functionDefaults {
          name        = Name $ toShort $ pack label
        , linkage     = L.External
        , parameters  = ([Parameter tpe name [] | (tpe, name) <- argTypes], False)
        , returnType  = returnType
        , basicBlocks = []
    }

entry :: Codegen Name
entry = gets currentBlock

uniqueName :: String -> Names -> (String, Names)
uniqueName name names =
    case Map.lookup name names of
        Nothing -> (name, Map.insert name 1 names)
        Just ix -> (name ++ show ix, Map.insert name (ix + 1) names)

emptyBlock :: Int -> BlockState
emptyBlock i = BlockState i [] Nothing

addBlock :: String -> Codegen Name
addBlock blockName = do
    blks <- gets blocks
    count <- gets blockCount
    nmes <- gets names

    let new             = emptyBlock count
        (uName, supply) = uniqueName blockName nmes

    modify $ \s -> s {
          blocks = Map.insert (Name $ toShort $ pack uName) new blks
        , blockCount = count + 1
        , names = supply
    }

    return (Name $ toShort $ pack uName)

setBlock :: Name -> Codegen Name
setBlock blockName = do
    modify $ \s -> s { currentBlock = blockName }
    return blockName

getBlock :: Codegen Name
getBlock = gets currentBlock

modifyBlock :: BlockState -> Codegen ()
modifyBlock new = do
    active <- gets currentBlock
    modify $ \s -> s { blocks = Map.insert active new (blocks s) }

current :: Codegen BlockState
current = do
    c <- gets currentBlock
    blks <- gets blocks
    case Map.lookup c blks of
        Just x -> return x
        Nothing -> error $ "No such block: " ++ show c

-- Add a fresh name supply
fresh :: Codegen Word
fresh = do
    i <- gets count
    modify $ \s -> s { count = 1 + i }
    return $ i + 1

local :: Name -> Operand
local = LocalReference Codegen.double

externf :: Int -> Name -> Operand
externf argCount = ConstantOperand . C.GlobalReference (PointerType {
      pointerReferent = FunctionType {
          resultType = Codegen.double
        , argumentTypes = take argCount (repeat Codegen.double)
        , isVarArg = False
      }
    , pointerAddrSpace = AddrSpace 0
})

-- the symTable keeps track of register names <-> register numbers
assign :: String -> Operand -> Codegen ()
assign var x = do
    lcls <- gets symTable
    modify $ \s -> s { symTable = [(var, x)] ++ lcls }

getvar :: String -> Codegen Operand
getvar var = do
    syms <- gets symTable
    case lookup var syms of
        Just x  -> return x
        Nothing -> error $ "Local variable not in scope: " ++ show var

-- pushing AST nodes onto the block stack
instr :: Instruction -> Codegen Operand
instr ins = do
    -- get a new "name"
    n <- fresh
    -- unnamed = numbered
    let ref = (UnName n)
    blk <- current
    -- get the instruction stack
    let i = stack blk
    modifyBlock (blk { stack = (ref := ins) : i } )
    return $ local ref

unnamedInstr :: Instruction -> Codegen ()
unnamedInstr ins = do
    blk <- current
    let i = stack blk
    modifyBlock (blk { stack = (Do ins) : i })
    return ()

-- terminators are rets & branches
terminator :: Named Terminator -> Codegen (Named Terminator)
terminator termintr = do
    blk <- current
    modifyBlock (blk { term = Just termintr })
    return termintr

makeBlock :: (Name, BlockState) -> BasicBlock
makeBlock (l, (BlockState _ s t)) = BasicBlock l (reverse s) (maketerm t)
    where
        maketerm (Just x) = x
        maketerm Nothing = error $ "Block has no terminator: " ++ (show l)

sortBlocks :: [(Name, BlockState)] -> [(Name, BlockState)]
sortBlocks = sortBy (compare `on` (idx . snd))

createBlocks :: CodegenState -> [BasicBlock]
createBlocks m = map makeBlock $ sortBlocks $ Map.toList (blocks m)

fadd :: Operand -> Operand -> Codegen Operand
fadd a b = instr $ FAdd noFastMathFlags a b []

fsub :: Operand -> Operand -> Codegen Operand
fsub a b = instr $ FSub noFastMathFlags a b []

fmul :: Operand -> Operand -> Codegen Operand
fmul a b = instr $ FMul noFastMathFlags a b []

fdiv :: Operand -> Operand -> Codegen Operand
fdiv a b = instr $ FDiv noFastMathFlags a b []

fcmp :: FP.FloatingPointPredicate -> Operand -> Operand -> Codegen Operand
fcmp cond a b = instr $ FCmp cond a b []

uitofp :: Type -> Operand -> Codegen Operand
uitofp tpe a = instr $ UIToFP a tpe []

-- branch
br :: Name -> Codegen (Named Terminator)
br val = terminator $ Do $ Br val []

-- conditional branch
cbr :: Operand -> Name -> Name -> Codegen (Named Terminator)
cbr cond tr fl = terminator $ Do $ CondBr cond tr fl []

-- return
ret :: Operand -> Codegen (Named Terminator)
ret val = terminator $ Do $ Ret (Just val) []

cmp :: FP.FloatingPointPredicate -> Operand -> Operand -> Codegen Operand
cmp pred a b = do
    test <- fcmp pred a b
    uitofp Codegen.double test

-- helper for transforming arg lists
toArgs :: [Operand] -> [(Operand, [A.ParameterAttribute])]
toArgs = map (\x -> (x, []))

call :: Operand -> [Operand] -> Codegen Operand
call fn args = instr $ Call Nothing CC.C [] (Right fn) (toArgs args) [] []

-- allocate on the stack
alloca :: Type -> Codegen Operand
alloca tpe = instr $ Alloca tpe Nothing 0 []

store :: Operand -> Operand -> Codegen ()
store ptr val = unnamedInstr $ Store False ptr val Nothing 0 []

load :: Operand -> Codegen Operand
load ptr = instr $ Load False ptr Nothing 0 []

entryBlockName :: String
entryBlockName = "luigi_entry"

execCodegen :: Codegen a -> CodegenState
execCodegen m = execState (runCodegen m) emptyCodegen

emptyCodegen :: CodegenState
emptyCodegen = CodegenState (Name $ toShort $ pack entryBlockName) Map.empty [] 1 0 Map.empty

const2operand :: C.Constant -> Operand
const2operand = ConstantOperand

phi :: Type -> [(Operand, Name)] -> Codegen Operand
phi tpe incoming = instr $ Phi tpe incoming []
