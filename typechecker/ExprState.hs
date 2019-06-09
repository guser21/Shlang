{-# LANGUAGE LambdaCase #-}

module ExprState where

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

getExprType :: Expr -> Result ValType
getExprType (EVar ident) = getTypeByIdent ident
getExprType (ELitInt integer) = return $ SimpleType Int
getExprType ELitTrue = return $ SimpleType Bool
getExprType ELitFalse = return $ SimpleType Bool
getExprType (EApp ident exprs) = do
  funVal <- getTypeByIdent ident
  case funVal of
    SimpleType (FuncType _ _) -> return ()
    _                         -> throwError $ "cannot call " ++ show ident
  let (SimpleType (FuncType argTypes type_)) = funVal
  let exprAndTypes = zip exprs argTypes
  if length argTypes /= length exprs
    then throwError $
         "wrong number of arguments supplied in funciton " ++ show ident
    else traverse_
           (\(expr, type_) --todo overshadowing the up type_
             -> do
              exprType <- getExprType expr
              when
                (exprType /= SimpleType type_)
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
         "cannot compare type " ++ show eType1 ++ " with " ++ show eType2
    else return $ SimpleType Bool
getExprType (EAnd expr1 expr2) = do
  eType1 <- getExprType expr1
  eType2 <- getExprType expr2
  unless
    (eType1 == eType2 || eType1 /= SimpleType Bool)
    (throwError $
     "invalidOperation with type " ++ show eType1 ++ " with " ++ show eType2)
  return $ SimpleType Bool
getExprType (EOr expr1 expr2) = getExprType (EAnd expr1 expr2)
getExprType (ELambda args type_ block) = do
  void (checkFunction (FnDef type_ lambdaIdent args block))
  return $ SimpleType (FuncType (argsToTypes args) type_)

----------------------Statements-----------------------------------------
getBlockType :: Block -> ValType -> Result ValType
getBlockType (Block stmts) expectedType = do
  newBlockId <- getNewBlockId
  stmtTypes <- local (makeNewBlock newBlockId) (getStmtType stmts expectedType)
  return $
    foldl
      (\acc curType ->
         if curType /= NoRetType
           then curType
           else acc)
      NoRetType
      stmtTypes

checkDeclTypes type_ =
  traverse_
    (\case
       NoInit ident -> return ()
       Init ident expr -> do
         newBlockId <- getNewBlockId
         expressionType <- local (makeNewBlock newBlockId) ( declValue ident (SimpleType type_) >>= \d -> d (getExprType expr))
         when
           (expressionType /= SimpleType type_)
           (throwError $ "incorrect declaration of type " ++ show type_))

checkIfConst :: Ident -> Result ()
checkIfConst ident = do
  (_, constNames, _, _) <- ask
  when
    (Set.member ident constNames)
    (throwError $ "cannot modify const variable " ++ show ident)

checkFunction (FnDef retType ident argDefs block) = do
  let argDecl =
        map (\(Arg argType argIdent) -> Decl argType [NoInit argIdent]) argDefs
  let Block stmts = block
  btype <-
    getBlockType (Block $ argDecl ++ stmts) (SimpleType retType) `catchError`
    (\e -> throwError $ "In function " ++ show ident ++ ": " ++ e)
  if btype == SimpleType retType || (btype == NoRetType && retType == Void)
    then return True
    else throwError $ "function " ++ show ident ++ " has a wrong return type"

getStmtType :: [Stmt] -> ValType -> Result [ValType]
getStmtType [] expectedType = return [NoRetType]
getStmtType (Empty:tl) expectedType =
  getStmtType tl expectedType >>= (\res -> return $ NoRetType : res)
getStmtType (BStmt block:tl) expectedType = do
  btype <- getBlockType block expectedType
  getStmtType tl expectedType >>= (\res -> return $ btype : res)
getStmtType (Decl type_ items:tl) expectedType = do
  checkDeclTypes type_ items
  declValueTypeLists [(type_, map getIdentFromItem items)] Set.empty >>=
    (\c -> c (getStmtType tl expectedType))
getStmtType (DeclFinal type_ items:tl) expectedType = do
  checkDeclTypes type_ items
  (env, consts, shadowVar, context) <- ask
  let idents = map getIdentFromItem items
  let newconsts = foldl (flip Set.insert) consts idents
  declCont <- declValueTypeLists [(type_, map getIdentFromItem items)] newconsts
  declCont (getStmtType tl expectedType)
getStmtType (Ass ident expr:tl) expectedType = do
  checkIfConst ident
  identType <- getTypeByIdent ident
  exprType <- getExprType expr
  if identType == exprType
    then getStmtType tl expectedType >>= (\res -> return $ NoRetType : res)
    else throwError $
         "cannot assgin " ++
         show exprType ++ " value to variable " ++ show ident
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
  getExprType expr >>= isPrintable >> getStmtType tl expectedType >>=
  (\res -> return $ NoRetType : res)
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
  checkIdent ident
  rememberIdent ident
  stmtType <-
    local
      (\(env, consts, curBlockId, context) ->
         (Map.insert ident l env, Set.insert ident consts, curBlockId, context))
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
getStmtType (FnInDef type_ ident args block:tl) expectedType = do
  declCont <- declValue ident (SimpleType (FuncType (argsToTypes args) type_))
  declCont
    (checkFunction (FnDef type_ ident args block) >> getStmtType tl expectedType)
