
module LambdaLifting where

import Control.Arrow
import Control.Monad.Writer
import qualified Parser as P

import qualified Data.Map as Map

-- Lift lambdas in the main block.
-- Returns a list of the lifted lambdas, followed by the modified main block.
liftLambdas :: P.Expression -> ([P.Expression], P.Expression)
liftLambdas block@(P.Block exprs) = (defs, main) where
    ((main, _), defs) = runWriter $ lambdaWriter [1..] block

lambdaWriter :: [Int] -> P.Expression -> Writer [P.Expression] (P.Expression, [Int])
lambdaWriter (i:is) (P.FunctionDefinition name args body) = do
    let fixedName = name -- ++ "_" ++ show i
        sub = Map.fromList $ [(name, P.FunctionReference fixedName)]
    (body, is) <- fmap (first (substitute sub)) $ lambdaWriter is body

    let def = P.FunctionDefinition fixedName args body
    tell [def]
    pure (P.FunctionReference fixedName, is)

lambdaWriter is (P.ImmutableBinding expr str) = do
    (body, is) <- lambdaWriter is expr
    pure (P.ImmutableBinding body str, is)

-- lambdaWriter is (P.MutableBinding expr str) = do
--     lambdaWriter is expr
--     pure (P.FunctionReference str, is)

-- lambdaWriter is (P.If expr1 expr2 Nothing) = do
--     lambdaWriter is expr1
--     lambdaWriter is expr2

-- lambdaWriter is (P.If expr1 expr2 (Just expr3)) = do
--     lambdaWriter is (P.If expr1 expr2 Nothing)
--     lambdaWriter is expr3

-- TODO: arithmetic & comparison

lambdaWriter is (P.Block exprs) = do
    forM (init exprs) $ \e -> do lambdaWriter is e
    lambdaWriter is (last exprs)

lambdaWriter is (P.Block [e]) = do
    pure (e, is)

lambdaWriter is expr = pure (expr, is)

substitute :: Map.Map String P.Expression -> P.Expression -> P.Expression
substitute mp expr = let ident = case expr of
                                    (P.Identifier ident) -> Just ident
                                    (P.FunctionCall ident _) -> Just ident
                                    (P.FunctionReference ident) -> Just ident
                                    _ -> Nothing
                      in case ident of
                            Just i -> case Map.lookup i mp of
                                        Just newRef -> newRef
                                        _           -> expr
                            Nothing -> expr
