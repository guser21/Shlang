module EvalStmt where

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
import           EvalDefs
import           EvalUtils
import           System.Exit            (exitFailure, exitSuccess)
import           System.IO

callStackLimit = 20000

declValueInit :: Type -> [Item] -> [Result Value]
declValueInit type_ =
  map
    (\it ->
       case it of
         (NoInit ident)    -> typeDefault type_
         (Init ident expr) -> evalExpr expr)

registerFunCall :: Ident -> Result ()
registerFunCall funIdent = do
  (st, l, scount) <- get
  when
    (scount > callStackLimit)
    (throwError $ "Stack Overflow" ++ show funIdent)
  modify (\(st, l, scount) -> (st, l, scount + 1))
  return ()

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
  --wrong implementation

--what if overshadows a variable
cleanMem :: Result ()
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

returnFromFunc :: a -> Result a
returnFromFunc val =
  modify (\(st, l, scount) -> (st, l, scount - 1)) >> return val

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
      case blockRes of
        Nothing       -> evalBlock tl
        Just finalVal -> return (Just finalVal)
    Decl type_ items -> do
      declCont <- declValueList (declIdents items) (declValueInit type_ items)
      declCont (evalBlock tl)
    FnInDef reType ident args block -> do
      declCont <-
        case reType of
          Void ->
            let withRetVoidFun =
                  FnDef reType ident args (Block [BStmt block, VRet])
             in declValue ident (return $ FunVal withRetVoidFun)
          _ -> declValue ident (return $ FunVal (FnDef reType ident args block))
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
