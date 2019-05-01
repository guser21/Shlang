module TypeChecker where

import           AbsDeclaration
import           EnvDefinitions

import           Control.Monad.Except
import           Control.Monad.Identity
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Foldable
import qualified Data.Map               as Map
import           Data.Maybe
import qualified Data.Set               as Set
import           System.Exit            (exitFailure, exitSuccess)
import           System.IO

data ValType
  = SimpleType { t :: Type }
  | NoRetType
  | FunType { fun :: TopDef }
  deriving (Eq)

instance Show ValType where
  show v =
    case v of
      FunType _    -> "function"
      NoRetType    -> "no return type"
      SimpleType t -> show t

type Loc = Integer

type ConstIdent = Set.Set Ident

type OverShadowableIdent = Set.Set Ident

type Env = (Map.Map Ident Loc, ConstIdent, OverShadowableIdent)

type Mem = Map.Map Loc ValType

type Store = (Mem, Loc)

type Result = ReaderT Env (StateT Store (ExceptT String IO))

newloc :: Result Loc
newloc = do
  (st, l) <- get
  put (st, l + 1)
  return l

modifyMem :: (Mem -> Mem) -> Result ()
modifyMem f = modify (\(st, l) -> (f st, l))

getTypeByLoc :: Loc -> Result ValType
getTypeByLoc loc = do
  (st, l) <- get
  case Map.lookup loc st of
    Just val -> return val
    Nothing  -> throwError "cannot find location for name"

getTypeByIdent :: Ident -> Result ValType
getTypeByIdent var = do
  (env, _, _) <- ask
  (st, l) <- get
  let varLoc = Map.lookup var env
  case varLoc of
    Just loc -> getTypeByLoc loc
    Nothing  -> throwError $ "unknown indentificator " ++ show var

typeDefault :: Type -> Result ValType
typeDefault type_ =
  case type_ of
    Int  -> return $ SimpleType Int
    Bool -> return $ SimpleType Bool
    Str  -> return $ SimpleType Str
    _    -> throwError "unrecognized type"

getIdentFromItem (NoInit ident)    = ident
getIdentFromItem (Init ident expr) = ident

declValue :: Ident -> Result ValType -> Result (Result a -> Result a)
declValue nameIdent resVal = do
  (env, constName, shadowable) <- ask
  when
    (Set.notMember nameIdent shadowable && Map.member nameIdent env)
    (throwError $ "name " ++ show nameIdent ++ " is already in use")
  l <- newloc
  val <- resVal
  modifyMem (Map.insert l val)
  return
    (local
       (\(env, constNames, shadowVar) ->
          (Map.insert nameIdent l env, constNames, shadowVar)))

mainFunc = Ident "main"

checkTypes :: Program -> Result ()
checkTypes (Program topDefs) = checkAllFunctions topDefs

checkAllFunctions (h:tl) = do
  let FnDef reType ident args block = h
  declCont <- declValue ident (return $ FunType h)
  declCont (checkAllFunctions tl)
checkAllFunctions [] = do
  (env, constName, _) <- ask
  traverse_
    (\(_, loc) -> getTypeByLoc loc >>= (checkFunction . fun))
    (Map.toList env)

checkFunction :: TopDef -> Result Bool
checkFunction (FnDef retType ident argDefs block) = do
  let argDecl =
        map (\(Arg argType argIdent) -> Decl argType [NoInit argIdent]) argDefs
  --todo check more if at least one return statement exists
  let Block stmts = block
  btype <- getBlockType (Block $ argDecl ++ stmts) (SimpleType retType)
  if btype == SimpleType retType || (btype == NoRetType && retType == Void)
    then return True
    else throwError $ "function " ++ show ident ++ " has a wrong return type"

--catch missmatching argument list size
--todo
getBlockType :: Block -> ValType -> Result ValType
getBlockType (Block stmts) expectedType = do
  stmtTypes <- getStmtType stmts expectedType
  return $
    foldl
      (\acc curType ->
         if curType /= NoRetType
           then curType
           else acc)
      NoRetType
      stmtTypes

markOvershadowable (nameLocks, consts, _) =
  (nameLocks, consts, Set.fromList $ Map.keys nameLocks)

getStmtType :: [Stmt] -> ValType -> Result [ValType]
getStmtType [] expectedType = return [NoRetType]
getStmtType (Empty:tl) expectedType =
  getStmtType tl expectedType >>= (\res -> return $ NoRetType : res)
getStmtType (BStmt block:tl) expectedType = do
  btype <- local markOvershadowable (getBlockType block expectedType)
  getStmtType tl expectedType >>= (\res -> return $ btype : res)
getStmtType (Decl type_ items:tl) expectedType = do
  typeCheck <-
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
      items
  when
    (not typeCheck)
    (throwError $ "incorrect declaration of type " ++ show type_)
  let definedType = typeDefault type_
  case items of
    (h:tailItems) ->
      let ident = getIdentFromItem h
       in do cont <- declValue ident definedType
             res <-
               cont (getStmtType (Decl type_ (tail items) : tl) expectedType)
             return $ NoRetType : res
    [] -> getStmtType tl expectedType >>= (\res -> return $ NoRetType : res)
getStmtType (DeclBlock type_ items:tl) expectedType =
  throwError "not implemented"
getStmtType (Ass ident expr:tl) expectedType = do
  identType <- getTypeByIdent ident
  exprType <- getExprType expr
  if identType /= exprType
    then throwError $
         "cannot assgin " ++
         show exprType ++ " value to variable " ++ show ident
    else getStmtType tl expectedType >>= (\res -> return $ NoRetType : res)
getStmtType (Incr ident:tl) expectedType = do
  identType <- getTypeByIdent ident
  if identType /= SimpleType Int
    then throwError $ "cannot increment type " ++ show identType
    else getStmtType tl expectedType >>= (\res -> return $ NoRetType : res)
getStmtType (Decr ident:tl) expectedType = do
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
  stmtType <- getBlockType (Block [stmt]) expectedType
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
  stmtType <- getBlockType (Block [stmt]) expectedType
  when
    (fromType /= SimpleType Int || toType /= SimpleType Int)
    (throwError "for bounds must be of type int")
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
  (FunType (FnDef type_ ident args block)) <- getTypeByIdent ident
  let argTypes = map (\(Arg type_ ident) -> SimpleType type_) args
  let exprAndTypes = zip exprs argTypes
  if length args /= length exprs
    then throwError $
         "wrong number of arguments supplied in funciton " ++ show ident
    else foldl
           (\acc (expr, type_) -> do
              exprType <- getExprType expr
              if exprType /= type_
                then throwError $
                     "wrong argument type in function " ++ show ident
                else return ())
           (return ())
           exprAndTypes
  return $ SimpleType type_
getExprType (EString string) = return $ SimpleType Str
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
getExprType (EMul expr1 mulop expr2) = do
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
      when
        (not
           ((eType1 == SimpleType Int && eType2 == SimpleType Int) ||
            (eType1 == SimpleType Str && eType2 == SimpleType Str)))
        (throwError $
         "cannot add expressions of type " ++ show eType1 ++ " " ++ show eType2)
    Minus ->
      when
        (not (eType1 == SimpleType Int && eType2 == SimpleType Int))
        (throwError $
         "cannot subtract expressions of type " ++
         show eType1 ++ " " ++ show eType2)
  return eType1
getExprType (ERel expr1 relop expr2) = do
  eType1 <- getExprType expr1
  eType2 <- getExprType expr2
  if eType1 /= eType2
    then throwError $
         "cannot compare type " ++ show eType1 ++ " with" ++ show eType2
    else return $ SimpleType Bool
getExprType (EAnd expr1 expr2) = do
  eType1 <- getExprType expr1
  eType2 <- getExprType expr2
  when
    (not (eType1 == eType2 || eType1 /= SimpleType Bool))
    (throwError $
     "invalidOperation with type " ++ show eType1 ++ " with" ++ show eType2)
  return $ SimpleType Bool
getExprType (EOr expr1 expr2) = getExprType (EAnd expr1 expr2)

checkProgramTypesIO :: Program -> IO Bool
checkProgramTypesIO prog = do
  ans <-
    runExceptT
      (runStateT
         (runReaderT (checkTypes prog) (Map.empty, Set.empty, Set.empty))
         (Map.empty, 0))
  case ans of
    (Left errMesg) -> putStrLn ("Type error: " ++ errMesg) >> return False
    _              -> return True --ended as supposed
