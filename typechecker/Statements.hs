module Statements where

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
import           Expressions
import           System.Exit            (exitFailure, exitSuccess)
import           System.IO
import           Utilities

getBlockType :: Block -> ValType -> Result ValType
getBlockType (Block stmts) expectedType = do
  stmtTypes <- local markOvershadowable (getStmtType stmts expectedType)
  return $
    foldl
      (\acc curType ->
         if curType /= NoRetType
           then curType
           else acc)
      NoRetType
      stmtTypes

markOvershadowable (nameLocks, consts, _, context) =
  (nameLocks, consts, Set.fromList $ Map.keys nameLocks, context)

checkDeclTypes type_ =
  foldl
    (\acc it ->
       case it of
         NoInit ident -> acc
         Init ident expr -> do
           (SimpleType exprType) <- getExprType expr
           acc >>=
             (\accVal ->
                return $ not (exprType == Void || exprType /= type_) && accVal))
    (return True)

checkIfConst :: Ident -> Result ()
checkIfConst ident = do
  (_, constNames, _, _) <- ask
  when
    (Set.member ident constNames)
    (throwError $ "cannot modify const variable " ++ show ident)

getStmtType :: [Stmt] -> ValType -> Result [ValType]
getStmtType [] expectedType = return [NoRetType]
getStmtType (Empty:tl) expectedType =
  getStmtType tl expectedType >>= (\res -> return $ NoRetType : res)
getStmtType (BStmt block:tl) expectedType = do
  btype <- local markOvershadowable (getBlockType block expectedType)
  getStmtType tl expectedType >>= (\res -> return $ btype : res)
  ---
getStmtType (Decl type_ items:tl) expectedType = do
  typeCheck <- checkDeclTypes type_ items
  unless typeCheck (throwError $ "incorrect declaration of type " ++ show type_)
  declCont <- declValueTypeLists [(type_, map getIdentFromItem items)] Set.empty
  declCont (getStmtType tl expectedType)
  ---
getStmtType (DeclFinal type_ items:tl) expectedType = do
  (env, consts, shadowVar, context) <- ask
  let idents = map getIdentFromItem items
  let newconsts = foldl (flip Set.insert) consts idents
  local
    (const (env, newconsts, shadowVar, context))
    (getStmtType (Decl type_ items : tl) expectedType)
getStmtType (Ass ident expr:tl) expectedType = do
  checkIfConst ident
  identType <- getTypeByIdent ident
  exprType <- getExprType expr
  if identType /= exprType
    then throwError $
         "cannot assgin " ++
         show exprType ++ " value to variable " ++ show ident
    else getStmtType tl expectedType >>= (\res -> return $ NoRetType : res)
getStmtType (Incr ident:tl) expectedType = do
  checkIfConst ident
  identType <- getTypeByIdent ident
  if identType /= SimpleType Int
    then throwError $ "cannot increment type " ++ show identType
    else getStmtType tl expectedType >>= (\res -> return $ NoRetType : res)
getStmtType (Decr ident:tl) expectedType = do
  checkIfConst ident
  identType <- getTypeByIdent ident
  if identType /= SimpleType Int
    then throwError $ "cannot decrement type " ++ show identType
    else getStmtType tl expectedType >>= (\res -> return $ NoRetType : res)
getStmtType (Ret expr:tl) expectedType = do
  exprType <- getExprType expr
  if exprType /= expectedType
    then throwError $ "incompatible function return type with " ++ show exprType
    else getStmtType tl expectedType >>= (\res -> return $ exprType : res)
getStmtType (VRet:tl) expectedType =
  if expectedType /= SimpleType Void
    then throwError "incompatible function type with return type void"
    else getStmtType tl expectedType >>=
         (\res -> return $ SimpleType Void : res)
getStmtType (Print expr:tl) expectedType =
  getStmtType tl expectedType >>= (\res -> return $ NoRetType : res)
getStmtType (Break:tl) expectedType = do
  (_, _, _, context) <- ask
  case context of
    WhileContext -> getStmtType tl expectedType
    OtherContext -> throwError "break statement is out of while context"
getStmtType (Continue:tl) expectedType = do
  (_, _, _, context) <- ask
  case context of
    WhileContext -> getStmtType tl expectedType
    OtherContext -> throwError "continue statement is out of while context"
getStmtType (Cond expr stmt:tl) expectedType = do
  stmtType <- getBlockType (Block [stmt]) expectedType
  condType <- getExprType expr
  when
    (condType /= SimpleType Bool)
    (throwError "incompatible type in if expression")
  let exprVal = fastEvalBool expr
  case exprVal of
    Nothing
      | stmtType == NoRetType || stmtType == expectedType ->
        getStmtType tl expectedType >>= (\res -> return $ NoRetType : res)
    (Just True)
      | stmtType == NoRetType || stmtType == expectedType ->
        getStmtType tl expectedType >>= (\res -> return $ stmtType : res)
    (Just False)
      | stmtType == NoRetType || stmtType == expectedType ->
        getStmtType tl expectedType
    _ ->
      throwError ("incompatible return type in if statement " ++ show stmtType)
getStmtType (While expr stmt:tl) expectedType = do
  exprType <- getExprType expr
  stmtType <-
    local
      (\(a1, a2, a3, _) -> (a1, a2, a3, WhileContext))
      (getBlockType (Block [stmt]) expectedType)
  when
    (exprType /= SimpleType Bool)
    (throwError "incompatible expression type in while condition")
  when
    (stmtType /= NoRetType && stmtType /= expectedType)
    (throwError "incompatible return type in while statement")
  getStmtType tl expectedType >>= (\res -> return $ NoRetType : res)
getStmtType (SExp expr:tl) expectedType =
  getExprType expr >> getStmtType tl expectedType >>=
  (\res -> return $ NoRetType : res)
getStmtType (ConstFor type_ ident exprFrom exprTo stmt:tl) expectedType = do
  fromType <- getExprType exprFrom
  toType <- getExprType exprTo
  when
    (fromType /= SimpleType Int || toType /= SimpleType Int)
    (throwError "for bounds must be of type int")
  l <- newloc
  modifyMem (Map.insert l (SimpleType Int))
  stmtType <-
    local
      (\(env, consts, shadowVar, context) ->
         (Map.insert ident l env, Set.insert ident consts, shadowVar, context))
      (getBlockType (Block [stmt]) expectedType)
  when
    (stmtType /= expectedType && stmtType /= NoRetType)
    (throwError "wrong return type in the for block")
  getStmtType tl expectedType >>= (\res -> return $ NoRetType : res)
getStmtType (CondElse expr stmt1 stmt2:tl) expectedType = do
  type1 <- getBlockType (Block [stmt1]) expectedType
  type2 <- getBlockType (Block [stmt2]) expectedType
  case fastEvalBool expr of
    Just True -> getStmtType tl expectedType >>= (\res -> return $ type1 : res)
    Just False -> getStmtType tl expectedType >>= (\res -> return $ type2 : res)
    Nothing ->
      case (type1, type2) of
        (NoRetType, NoRetType) ->
          getStmtType tl expectedType >>= (\res -> return $ NoRetType : res)
        (_, NoRetType)
          | type1 == expectedType ->
            getStmtType tl expectedType >>= (\res -> return $ NoRetType : res)
        (NoRetType, _)
          | type2 == expectedType ->
            getStmtType tl expectedType >>= (\res -> return $ NoRetType : res)
        (t1, t2)
          | t1 == t2 && t1 == expectedType ->
            getStmtType tl expectedType >>= (\res -> return $ t1 : res)
        _ -> throwError "incompatible return type in if else condition"
