module EvaluateProgram where

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

data Value
  = BoolVal { bool :: Bool }
  | NumVal { num :: Integer }
  | StrVal { str :: String }
  | FunVal { fun :: TopDef }
  | VoidVal
  | BreakVal
  | ContVal
  deriving (Eq)

instance Show Value where
  show v =
    case v of
      BoolVal b -> show b
      NumVal n  -> show n
      StrVal s  -> s
      FunVal f  -> show f
      BreakVal  -> "breakVal"
      ContVal   -> "contVal "
      VoidVal   -> "voidVal"

type Loc = Integer

-- Ident-> loc, break,continue
type Env = Map.Map Ident Loc

type Mem = Map.Map Loc Value

type StackCount = Integer

type Store = (Mem, Loc, StackCount)

type Result = ReaderT Env (StateT Store (ExceptT String IO))

callStackLimit = 20000

registerFunCall :: Ident -> Result ()
registerFunCall funIdent = do
  (st, l, scount) <- get
  when
    (scount > callStackLimit)
    (throwError $ "Stack Overflow" ++ show funIdent)
  modify (\(st, l, scount) -> (st, l, scount + 1))
  return ()

returnFromFunc :: a -> Result a
returnFromFunc val =
  modify (\(st, l, scount) -> (st, l, scount - 1)) >> return val

newloc :: Result Loc
newloc = do
  (st, l, scount) <- get
  put (st, l + 1, scount)
  return l

modifyMem :: (Mem -> Mem) -> Result ()
modifyMem f = modify (\(st, l, scount) -> (f st, l, scount))

getValByLoc :: Loc -> Result Value
getValByLoc loc = do
  (st, l, scount) <- get
  case Map.lookup loc st of
    Just val -> return val
    Nothing  -> throwError "cannot find location for name"

getValByIdent :: Ident -> Result Value
getValByIdent var = do
  env <- ask
  (st, l, _) <- get
  let varLoc = Map.lookup var env
  case varLoc of
    Just loc -> getValByLoc loc
    Nothing  -> throwError $ "unknown indentificator " ++ show var

modifyVariable :: Ident -> (Value -> Value) -> Result ()
modifyVariable var f = do
  env <- ask
  (st, l, _) <- get
  let (Ident varname) = var
  let varLoc = Map.lookup var env
  case varLoc of
    Just loc -> getValByLoc loc >>= (modifyMem . Map.insert loc . f)
    Nothing  -> throwError $ "unknown indentificator " ++ show var

typeDefault :: Type -> Result Value
typeDefault type_ =
  case type_ of
    Int  -> return $ NumVal 0
    Bool -> return $ BoolVal True
    Str  -> return $ StrVal ""
    _    -> throwError "unrecognized type"

declVars :: Type -> [Ident] -> Result (Result a -> Result a)
declVars type_ nameIdents =
  declValueList nameIdents (map (\_ -> typeDefault type_) nameIdents)

declVar :: Type -> Ident -> Result (Result a -> Result a)
declVar type_ nameIdent = declValue nameIdent (typeDefault type_)

declValue :: Ident -> Result Value -> Result (Result a -> Result a)
declValue nameIdent resVal = do
  l <- newloc
  val <- resVal
  modifyMem (Map.insert l val)
  return (local (Map.insert nameIdent l))

declValueList :: [Ident] -> [Result Value] -> Result (Result a -> Result a)
declValueList (fn:nameIdents) (fv:values) = do
  declCont <- declValue fn fv
  declNextCont <- declValueList nameIdents values
  return $ declCont . declNextCont
declValueList [] [] = return (local id)
declValueList [] (h:t) = throwError "Mismatching argument list size"
declValueList (h:t) [] = throwError "Mismatching argument list size"

mainFunc = Ident "main"

runProgram :: Program -> Result ()
runProgram (Program topDefs) = runFunctions topDefs

runFunctions :: [TopDef] -> Result ()
runFunctions (h:tl) = do
  declCont <-
    case h
      --add return statement to void functions for stack counting
          of
      FnDef reType ident args block ->
        case reType of
          Void ->
            let withRetVoidFun =
                  FnDef reType ident args (Block [BStmt block, VRet])
             in declValue ident (return $ FunVal withRetVoidFun)
          _ -> declValue ident (return $ FunVal h)
      GlobDecl type_ items ->
        declValueList (declIdents items) (declValueInit type_ items)
      GlobFinDecl type_ items ->
        declValueList (declIdents items) (declValueInit type_ items)
  declCont (runFunctions tl)
runFunctions [] = do
  env <- ask
  case Map.lookup mainFunc env of
    Just mainLoc -> do
      funRes <- getValByLoc mainLoc
      let FunVal mainFun = funRes
      evalFunction mainFun []
      return ()
    Nothing -> throwError "Cannot find definition of function main"

runProgramIO :: Program -> IO ()
runProgramIO prog = do
  ans <-
    runExceptT
      (runStateT (runReaderT (runProgram prog) Map.empty) (Map.empty, 0, 0))
  case ans of
    (Left errMesg) -> putStrLn $ "Runtime error: " ++ errMesg
    _              -> return () --ended as supposed

-- --Todo typecheckint
evalFunction :: TopDef -> [Result Value] -> Result Value
evalFunction (FnDef funType funName argDefs block) argVals = do
  let argIdent = map (\(Arg argType argIdent) -> argIdent) argDefs
  let Block stmts = block
  let Ident rawName = funName
  funArgDeclCont <- declValueList argIdent argVals
  resVal <- funArgDeclCont (evalBlock stmts)
  case resVal of
    Nothing  -> return VoidVal
    Just val -> return val

declValueInit :: Type -> [Item] -> [Result Value]
declValueInit type_ =
  map
    (\it ->
       case it of
         (NoInit ident)    -> typeDefault type_
         (Init ident expr) -> evalExpr expr)

declIdents :: [Item] -> [Ident]
declIdents =
  map
    (\it ->
       case it of
         NoInit ident    -> ident
         Init ident expr -> ident)

cleanMem = do
  env <- ask
  (mem, loc, stackCount) <- get
  let nMem =
        foldl
          (\acc (ident, loc) ->
             case Map.lookup loc mem of
               Nothing     -> acc
               Just curVal -> Map.insert loc curVal acc)
          Map.empty
          (Map.toList env)
   in put (nMem, loc, stackCount)

runForBody curInd loc end stmt =
  when
    (curInd <= end)
    (modifyMem (Map.insert loc (NumVal curInd)) >>
     evalBlock [BStmt $ Block [stmt]] >>
     runForBody (curInd + 1) loc end stmt)

runWhile expr stmt = do
  BoolVal e <- evalExpr expr
  if e
    then evalBlock [BStmt $ Block [stmt]] >>=
      -- for the case someone broke out of the loop etc
         (\res ->
            case res of
              Nothing       -> runWhile expr stmt
              Just ContVal  -> runWhile expr stmt
              Just BreakVal -> return Nothing
              Just _        -> return res)
    else return Nothing

evalBlock :: [Stmt] -> Result (Maybe Value)
evalBlock (h:tl) =
  case h of
    Empty -> evalBlock tl
    BStmt (Block stmts) -> do
      blockRes <- local id (evalBlock stmts)
      cleanMem
      case blockRes of
        Nothing       -> evalBlock tl
        Just finalVal -> return (Just finalVal)
    Decl type_ items -> do
      declCont <- declValueList (declIdents items) (declValueInit type_ items)
      declCont (evalBlock tl)
    DeclFinal type_ items -> evalBlock (Decl type_ items : tl)
    Ass ident expr ->
      evalExpr expr >>= (modifyVariable ident . const) >> evalBlock tl
    Incr ident ->
      evalBlock $ Ass ident (EAdd (EVar ident) Plus (ELitInt 1)) : tl
    Decr ident ->
      evalBlock $ Ass ident (EAdd (EVar ident) Minus (ELitInt 1)) : tl
    ConstFor type_ ident expr1 expr2 stmt -> do
      (NumVal start) <- evalExpr expr1
      (NumVal end) <- evalExpr expr2
      loc <- newloc
      local (Map.insert ident loc) (runForBody start loc end stmt) >>
        evalBlock tl
    Ret expr -> (Just <$> evalExpr expr) >>= returnFromFunc
    VRet -> returnFromFunc (Just VoidVal)
    Print expr -> evalExpr expr >>= (liftIO . print) >> evalBlock tl
    Cond expr stmt -> evalBlock $ CondElse expr stmt Empty : tl
    CondElse expr stmt1 stmt2 ->
      let evalAsBlock stmt = evalBlock $ BStmt (Block [stmt]) : tl
       in evalExpr expr >>=
          (\(BoolVal cond) ->
             if cond
               then evalAsBlock stmt1
               else evalAsBlock stmt2)
    While expr stmt -> do
      res <- runWhile expr stmt
      case res of
        Nothing -> evalBlock tl
        Just _  -> return res
    Break -> return $ Just BreakVal
    Continue -> return $ Just ContVal
    SExp expr -> evalExpr expr >> evalBlock tl
evalBlock [] = return Nothing

evalExpr :: Expr -> Result Value
evalExpr x =
  case x of
    EVar ident -> getValByIdent ident
    ELitInt integer -> return (NumVal integer)
    ELitTrue -> return (BoolVal True)
    ELitFalse -> return (BoolVal False)
    EApp ident args -> do
      registerFunCall ident
      let argRes = map evalExpr args
      (FunVal fun) <- getValByIdent ident
      evalFunction fun argRes
    EString string -> return (StrVal string)
    Neg expr -> evalExpr expr >>= (\(NumVal v) -> return $ NumVal (-v))
    Not expr -> evalExpr expr >>= (\(BoolVal v) -> return $ BoolVal (not v))
    EMul expr1 mulop expr2 -> do
      (NumVal v1) <- evalExpr expr1
      (NumVal v2) <- evalExpr expr2
      case mulop of
        Times -> return $ NumVal (v1 * v2)
        Div ->
          if v2 == 0
            then throwError "cannot divide by 0"
            else return $ NumVal (v1 `quot` v2)
        Mod ->
          if v2 == 0
            then throwError "cannot evaluate mod by 0"
            else return $ NumVal (v1 `mod` v2)
    EAdd expr1 addop expr2 -> do
      v1 <- evalExpr expr1
      v2 <- evalExpr expr2
      addVal v1 v2 addop
    ERel expr1 relop expr2 -> evalRelOp expr1 relop expr2
    EAnd expr1 expr2 -> do
      (BoolVal v1) <- evalExpr expr1
      if not v1
        then return $ BoolVal False
        else evalExpr expr2
    EOr expr1 expr2 -> do
      (BoolVal v1) <- evalExpr expr1
      if v1
        then return $ BoolVal True
        else evalExpr expr2

evalArgs expr1 expr2 f = do
  v1 <- evalExpr expr1
  v2 <- evalExpr expr2
  return $ f v1 v2

addVal :: Value -> Value -> AddOp -> Result Value
addVal (NumVal v1) (NumVal v2) addop =
  case addop of
    Plus  -> return $ NumVal (v1 + v2)
    Minus -> return $ NumVal (v1 - v2)
addVal (StrVal v1) (StrVal v2) addop =
  case addop of
    Plus  -> return $ StrVal (v1 ++ v2)
    Minus -> throwError "subtraction not supported on strings"

lessThan (BoolVal b1) (BoolVal b2) = b1 < b2
lessThan (StrVal s1) (StrVal s2)   = s1 < s2
lessThan (NumVal n1) (NumVal n2)   = n1 < n2

equal (BoolVal b1) (BoolVal b2) = b1 == b2
equal (StrVal s1) (StrVal s2)   = s1 == s2
equal (NumVal n1) (NumVal n2)   = n1 == n2

evalRelOp :: Expr -> RelOp -> Expr -> Result Value
evalRelOp expr1 relop expr2 = do
  l <- evalExpr expr1
  r <- evalExpr expr2
  let res =
        case relop of
          LTH -> lessThan l r
          LE  -> lessThan l r || equal l r
          GTH -> lessThan r l
          GE  -> lessThan r l || equal l r
          EQU -> equal l r
          NE  -> not $ equal l r
   in return (BoolVal res)
