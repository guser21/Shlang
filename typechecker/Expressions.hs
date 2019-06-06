module Expressions where

import           AbsDeclaration
import           EnvDefinitions

import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Identity
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Foldable
import qualified Data.Map               as Map
import           Data.Maybe
import qualified Data.Set               as Set
import           Definitions
import           System.Exit            (exitFailure, exitSuccess)
import           System.IO
import           Utilities

fastEvalBool :: Expr -> Maybe Bool
fastEvalBool expr =
  case expr of
    ELitTrue  -> Just True
    ELitFalse -> Just False
    _         -> Nothing

getExprType :: Expr -> Result ValType
getExprType (EVar ident) = getTypeByIdent ident
getExprType (ELitInt integer) = return $ SimpleType Int
getExprType ELitTrue = return $ SimpleType Bool
getExprType ELitFalse = return $ SimpleType Bool
getExprType (EApp ident exprs) = do
  funVal <- getTypeByIdent ident
  case funVal of
    FunType _ -> return ()
    _         -> throwError $ "cannot call " ++ show ident
  let (FunType (FnDef type_ ident args block)) = funVal
  let argTypes = map (\(Arg argType _) -> SimpleType argType) args
  let exprAndTypes = zip exprs argTypes
  if length args /= length exprs
    then throwError $
         "wrong number of arguments supplied in funciton " ++ show ident
    else traverse_
           (\(expr, type_) --todo overshadowing the up type_
             -> do
              exprType <- getExprType expr
              when
                (exprType /= type_)
                (throwError $ "wrong argument type in function " ++ show ident))
           exprAndTypes
  return $ SimpleType type_
getExprType (EString _) = return $ SimpleType Str
getExprType (Neg expr) = do
  eType <- getExprType expr
  when
    (eType /= SimpleType Int)
    (throwError $ "cannot negate an expression of type " ++ show eType)
  return $ SimpleType Int
getExprType (Not expr) = do
  eType <- getExprType expr
  when
    (eType /= SimpleType Bool)
    (throwError $ "cannot invert an expression of type " ++ show eType)
  return $ SimpleType Bool
getExprType (EMul expr1 _ expr2) = do
  eType1 <- getExprType expr1
  eType2 <- getExprType expr2
  when
    (eType1 /= SimpleType Int || eType2 /= SimpleType Int)
    (throwError $
     "invalid operation with type " ++ show eType1 ++ " " ++ show eType2)
  return $ SimpleType Int
getExprType (EAdd expr1 addop expr2) = do
  eType1 <- getExprType expr1
  eType2 <- getExprType expr2
  case addop of
    Plus ->
      unless
        ((eType1 == SimpleType Int && eType2 == SimpleType Int) ||
         (eType1 == SimpleType Str && eType2 == SimpleType Str))
        (throwError $
         "cannot add expressions of type " ++ show eType1 ++ " " ++ show eType2)
    Minus ->
      unless
        (eType1 == SimpleType Int && eType2 == SimpleType Int)
        (throwError $
         "cannot subtract expressions of type " ++
         show eType1 ++ " " ++ show eType2)
  return eType1
getExprType (ERel expr1 _ expr2) = do
  eType1 <- getExprType expr1
  eType2 <- getExprType expr2
  if eType1 /= eType2
    then throwError $
         "cannot compare type " ++ show eType1 ++ " with" ++ show eType2
    else return $ SimpleType Bool
getExprType (EAnd expr1 expr2) = do
  eType1 <- getExprType expr1
  eType2 <- getExprType expr2
  unless
    (eType1 == eType2 || eType1 /= SimpleType Bool)
    (throwError $
     "invalidOperation with type " ++ show eType1 ++ " with" ++ show eType2)
  return $ SimpleType Bool
getExprType (EOr expr1 expr2) = getExprType (EAnd expr1 expr2)
