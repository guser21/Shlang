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
      NoRetType    -> "np return type"
      SimpleType t -> show t

type Loc = Integer

type ConstIdent = Set.Set Ident

type Env = (Map.Map Ident Loc, ConstIdent)

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
  (env, _) <- ask
  (st, l) <- get
  let varLoc = Map.lookup var env
  case varLoc of
    Just loc -> getTypeByLoc loc
    Nothing  -> throwError $ "unknown indentificator " ++ show var

modifyVariable :: Ident -> (ValType -> ValType) -> Result ()
modifyVariable var f = do
  (env, constNames) <- ask
  (st, l) <- get
  let (Ident varname) = var
  let varLoc = Map.lookup var env
  if Set.member var constNames
    then throwError $ "cannot modify const variable " ++ varname
    else (case varLoc of
            Just loc -> getTypeByLoc loc >>= (modifyMem . Map.insert loc . f)
            Nothing  -> throwError $ "unknown indentificator " ++ show var)

typeDefault :: Type -> Result ValType
typeDefault type_ =
  case type_ of
    Int  -> return $ SimpleType Int
    Bool -> return $ SimpleType Bool
    Str  -> return $ SimpleType Str
    _    -> throwError "unrecognized type"

declVars :: Type -> [Ident] -> Result (Result a -> Result a)
declVars type_ nameIdents =
  declValueList nameIdents (map (\_ -> typeDefault type_) nameIdents)

declVar :: Type -> Ident -> Result (Result a -> Result a)
declVar type_ nameIdent = declValue nameIdent (typeDefault type_)

declIdents :: [Item] -> [Ident]
declIdents =
  map
    (\it ->
       case it of
         NoInit ident    -> ident
         Init ident expr -> ident)

declValue :: Ident -> Result ValType -> Result (Result a -> Result a)
declValue nameIdent resVal = do
  l <- newloc
  val <- resVal
  modifyMem (Map.insert l val)
  return
    (local (\(env, constNames) -> (Map.insert nameIdent l env, constNames)))

declValueList :: [Ident] -> [Result ValType] -> Result (Result a -> Result a)
declValueList (fn:nameIdents) (fv:values) = do
  declCont <- declValue fn fv
  declNextCont <- declValueList nameIdents values
  return $ declCont . declNextCont
declValueList [] [] = return (local id)
declValueList [] (h:t) = throwError "Mismatching argument list size"
declValueList (h:t) [] = throwError "Mismatching argument list size"

declTypeInit :: Type -> [Item] -> [Result ValType]
declTypeInit type_ = map (\it -> typeDefault type_)

mainFunc = Ident "main"

checkProgramTypesIO :: Program -> IO Bool
checkProgramTypesIO prog = do
  ans <-
    runExceptT
      (runStateT
         (runReaderT (checkTypes prog) (Map.empty, Set.empty))
         (Map.empty, 0))
  case ans of
    (Left errMesg) -> putStrLn ("Type error: " ++ errMesg) >> return False
    _              -> return True --ended as supposed

checkTypes :: Program -> Result ()
checkTypes (Program topDefs) = checkAllFunctions topDefs

checkAllFunctions (h:tl) = do
  let FnDef reType ident args block = h
  declCont <- declValue ident (return $ FunType h)
  declCont (checkAllFunctions tl)
checkAllFunctions [] = do
  (env, constName) <- ask
  traverse_
    (\(_, loc) -> getTypeByLoc loc >>= (checkFunction . fun))
    (Map.toList env)

checkFunction :: TopDef -> Result Bool
checkFunction (FnDef retType ident argDefs block) = do
  let argIdents = map (\(Arg argType argIdent) -> argIdent) argDefs
  let argTypes = map (\(Arg argType argIdent) -> typeDefault argType) argDefs
  funArgDeclCont <- declValueList argIdents argTypes
  --todo check more if at least one return statement exists
  btype <- funArgDeclCont (getBlockType block (SimpleType retType))
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

getStmtType :: [Stmt] -> ValType -> Result [ValType]
--todo declare in environment
getStmtType [] expectedType = return [NoRetType]
getStmtType (stmt:tl) expectedType =
  case stmt of
    Empty -> getStmtType tl expectedType >>= (\res -> return $ NoRetType : res)
    BStmt block -> do
      btype <- getBlockType block expectedType
      getStmtType tl expectedType >>= (\res -> return $ btype : res)
    Decl type_ items -> do
      typeCheck <-
        foldl
          (\acc it ->
             case it of
               NoInit ident -> acc
               Init ident expr -> do
                 (SimpleType exprType) <- getExprType expr
                 accVal <- acc
                 return
                   ((not (exprType == Void || exprType /= type_)) && accVal))
          (return True)
          items
      when
        (not typeCheck)
        (throwError $ "incorrect declaration of type" ++ show type_)
      declCont <- declValueList (declIdents items) (declTypeInit type_ items)
      returnTypes <- declCont (getStmtType tl expectedType)
      return $ NoRetType : returnTypes
    DeclBlock type_ items -> throwError "not implemented"
    Ass ident expr -> do
      identType <- getTypeByIdent ident
      exprType <- getExprType expr
      if identType /= exprType
        then throwError $
             "cannot assgin " ++
             show exprType ++ "value to variable" ++ show ident
        else getStmtType tl expectedType >>= (\res -> return $ NoRetType : res)
    Incr ident -> do
      identType <- getTypeByIdent ident
      if identType /= SimpleType Int
        then throwError $ "cannot increment type " ++ show identType
        else getStmtType tl expectedType >>= (\res -> return $ NoRetType : res)
    Decr ident -> do
      identType <- getTypeByIdent ident
      if identType /= SimpleType Int
        then throwError $ "cannot decrement type " ++ show identType
        else getStmtType tl expectedType >>= (\res -> return $ NoRetType : res)
    Ret expr -> do
      exprType <- getExprType expr
      if exprType /= expectedType
        then throwError $
             "incompatible function return type with" ++ show exprType
        else getStmtType tl expectedType >>= (\res -> return $ exprType : res)
    VRet ->
      if expectedType /= SimpleType Void
        then throwError "incompatible function type with return type void"
        else getStmtType tl expectedType >>=
             (\res -> return $ SimpleType Void : res)
    Print expr ->
      getStmtType tl expectedType >>= (\res -> return $ NoRetType : res)
    Cond expr stmt -> do
      stmtType <- getBlockType (Block [stmt]) expectedType
      if (stmtType == NoRetType && expectedType == SimpleType Void) ||
         (stmtType == expectedType)
        then getStmtType tl expectedType >>= (\res -> return $ NoRetType : res)
        else throwError
               ("incompatible return type in if statement" ++ show stmtType)
    CondElse expr stmt1 stmt2 -> do
      type1 <- getBlockType (Block [stmt1]) expectedType
      type2 <- getBlockType (Block [stmt2]) expectedType
      if (type1 == type2) && (expectedType == type1)
        then getStmtType tl expectedType >>= (\res -> return $ type1 : res)
        else if (type1 == NoRetType || type1 == SimpleType Void) &&
                (type2 == NoRetType || type2 == SimpleType Void) &&
                (expectedType == SimpleType Void)
               then getStmtType tl expectedType >>=
                    (\res -> return $ SimpleType Void : res)
               else throwError "incompatible return type in if else condition"
    While expr stmt -> do
      exprType <- getExprType expr
      stmtType <- getBlockType (Block [stmt]) expectedType
      when
        (exprType /= SimpleType Bool)
        (throwError "incompatible expression type in while condition")
      when
        (stmtType /= NoRetType && stmtType /= expectedType)
        (throwError "incompatible return type in while statement")
      getStmtType tl expectedType >>= (\res -> return $ NoRetType : res)
    SExp expr ->
      getStmtType tl expectedType >>= (\res -> return $ NoRetType : res)
    ConstFor type_ ident exprFrom exprTo stmt -> do
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

getExprType :: Expr -> Result ValType
getExprType expr =
  case expr of
    EVar ident -> getTypeByIdent ident
    ELitInt integer -> return $ SimpleType Int
    ELitTrue -> return $ SimpleType Bool
    ELitFalse -> return $ SimpleType Bool
    EApp ident exprs -> do
      (FunType (FnDef type_ ident args block)) <- getTypeByIdent ident
      when
        (length args /= length args)
        (throwError $
         "wrong number of arguments supplied in funciton" ++ show ident)
      let argTypes = map (\(Arg type_ ident) -> SimpleType type_) args
      let exprAndTypes = zip exprs argTypes
      traverse_
        (\(expr, type_) -> do
           exprType <- getExprType expr
           when
             (exprType /= type_)
             (throwError $ "wrong argument type in function" ++ show ident))
        exprAndTypes
      return $ SimpleType type_
    EString string -> return $ SimpleType Str
    Neg expr -> do
      eType <- getExprType expr
      when
        (eType /= SimpleType Int)
        (throwError $ "cannot negate an expression of type " ++ show eType)
      return $ SimpleType Int
    Not expr -> do
      eType <- getExprType expr
      when
        (eType /= SimpleType Bool)
        (throwError $ "cannot invert an expression of type " ++ show eType)
      return $ SimpleType Bool
    EMul expr1 mulop expr2 -> do
      eType1 <- getExprType expr1
      eType2 <- getExprType expr2
      when
        (eType1 /= SimpleType Int || eType2 /= SimpleType Int)
        (throwError $
         "invalid operation with type" ++ show eType1 ++ " " ++ show eType2)
      return $ SimpleType Int
    EAdd expr1 addop expr2 -> do
      eType1 <- getExprType expr1
      eType2 <- getExprType expr2
      case addop of
        Plus ->
          when
            (not
               ((eType1 == SimpleType Int && eType2 == SimpleType Int) ||
                (eType1 == SimpleType Str && eType2 == SimpleType Str)))
            (throwError $
             "cannot add expressions of type " ++
             show eType1 ++ " " ++ show eType2)
        Minus ->
          when
            (not (eType1 == SimpleType Int && eType2 == SimpleType Int))
            (throwError $
             "cannot subtract expressions of type " ++
             show eType1 ++ " " ++ show eType2)
      return eType1
    ERel expr1 relop expr2 -> do
      eType1 <- getExprType expr1
      eType2 <- getExprType expr2
      when
        (eType1 /= eType2)
        (throwError $
         "cannot compare type " ++ show eType1 ++ " with" ++ show eType2)
      return $ SimpleType Bool
    EAnd expr1 expr2 -> do
      eType1 <- getExprType expr1
      eType2 <- getExprType expr2
      when
        (not (eType1 == eType2 || eType1 /= SimpleType Bool))
        (throwError $
         "invalidOperation with type " ++ show eType1 ++ " with" ++ show eType2)
      return $ SimpleType Bool
    EOr expr1 expr2 -> getExprType (EAnd expr1 expr2)
