module EvaluateProgram where

import EnvDefinitions
import AbsDeclaration

import           Control.Monad.Identity
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Maybe
import qualified Data.Map                      as Map
import System.IO
import System.Exit ( exitFailure, exitSuccess )

data Value 
  = BoolVal {bool :: Bool} 
  | NumVal {num :: Integer} 
  | StrVal {str :: String} 
  | FunVal {fun :: TopDef }
  | VoidVal 
   deriving (Eq)  

instance Show Value where
  show v = case v of 
    BoolVal b -> show b
    NumVal n -> show n
    StrVal s ->s
    FunVal f ->show f
    VoidVal ->"voidVal"

 
type Loc = Integer

type Env = Map.Map Ident Loc
type Mem = Map.Map Loc Value

type Store = (Mem,Loc)

type Result = ReaderT Env (StateT Store (ExceptT String IO)) 


newloc :: Result Loc
newloc = do
  (st,l) <- get
  put (st,l+1)
  return l

modifyMem :: (Mem -> Mem) -> Result ()
modifyMem f = modify (\(st,l) -> (f st,l))

getValByLoc :: Loc -> Result Value
getValByLoc loc =do
  (st,l) <- get
  case Map.lookup loc st of
    Just val-> return val 
    Nothing -> throwError "cannot find location for name"

getValByIdent ::Ident -> Result Value
getValByIdent var = do
  env <- ask
  (st,l) <- get
  let varLoc = Map.lookup var env
  case varLoc of 
    Just loc ->  getValByLoc loc
    Nothing -> throwError $ "unknown indentificator "++ (show var)

modifyVariable :: Ident -> (Value -> Value )-> Result ()
modifyVariable var f =do 
  env <- ask
  (st,l) <- get
  let varLoc = Map.lookup var env
  case varLoc of 
    Just loc ->  (getValByLoc loc) >>= (\val -> modifyMem (\curSt-> Map.insert loc (f val) curSt))
    Nothing -> throwError $ "unknown indentificator "++ (show var)

typeDefault :: Type -> Result Value
typeDefault type_ = case type_ of
  Int -> return $ NumVal 0
  Bool -> return $ BoolVal True 
  Str -> return $  StrVal "" 
  _ -> throwError "unrecognized type"

declVars :: Type -> [Ident]-> Result (Result a -> Result a)
declVars type_ nameIdents = declValueList nameIdents (map (\_-> typeDefault type_ ) nameIdents) 

declVar :: Type -> Ident-> Result (Result a -> Result a)
declVar type_ nameIdent = declValue nameIdent (typeDefault type_)

declValue :: Ident -> (Result Value) -> Result (Result a -> Result a)
declValue nameIdent resVal =  do
    l <- newloc;
    val<-resVal;
    modifyMem (Map.insert l val);
    return (local (Map.insert nameIdent l))

declValueList :: [Ident] -> [Result Value] -> Result (Result a -> Result a)
declValueList (fn:nameIdents) (fv:values) = do
  declCont <- declValue fn fv
  declNextCont <- declValueList nameIdents values
  return $ declCont . declNextCont
declValueList [] [] = return $ (local id) 

declValueList [] (h:t) = throwError "Mismatching argument list size"
declValueList (h:t) [] = throwError "Mismatching argument list size"



mainFunc =Ident "main"
  
runProgram ::Program -> Result ()
runProgram (Program topDefs) = runFunctions topDefs

runFunctions :: [TopDef] -> Result ()
runFunctions (h:tl) = do 
  let FnDef reType ident args block = h 
  declCont <- declValue ident (return $ FunVal h)
  declCont (runFunctions tl)


runFunctions [] = do
  env <- ask;
  case  (Map.lookup mainFunc env) of
    --call main
    Just mainLoc -> do
      funRes <- getValByLoc mainLoc
      let FunVal mainFun =funRes
      --super ugly
      evalFunction mainFun []
      return ()
    Nothing -> throwError "Cannot find definition of function main"


runProgramIO :: Program-> IO ()
runProgramIO prog = do
   ans <- runExceptT (runStateT (runReaderT (runProgram prog) Map.empty) (Map.empty,0)) ; 
   case ans of 
    (Left errMesg) -> putStrLn $ "Runtime error: " ++ errMesg
    _ -> return () --ended as supposed



-- --Todo typecheckint
evalFunction :: TopDef -> [Result Value] -> Result Value
evalFunction (FnDef funType funName argDefs block) argVals  = do
  let argIdent =  map (\(Arg argType argIdent)-> argIdent) argDefs 
  let Block stmts = block
  funArgDeclCont <- declValueList argIdent argVals
  resVal <- funArgDeclCont (evalBlock stmts)
  case resVal of 
    Nothing -> throwError $ "No return statemant in " ++ (let Ident rawName= funName in rawName)   
    Just val -> return val 
  
--what if returns from an inside block
-- {
--   {
--     return 5
--   }
-- }

declValueInit :: Type -> [Item] -> ([Result Value])  
declValueInit type_ items =  map (\it ->do 
  case it of 
    (NoInit ident) -> typeDefault type_
    (Init ident expr) -> evalExpr expr) items

declIdents :: [Item] -> [Ident]
declIdents items =  map (\it -> case it of 
  NoInit ident -> ident
  Init ident expr -> ident) items


evalBlock :: [Stmt] -> Result (Maybe Value)
evalBlock (h:tl) = case h of
  Empty -> evalBlock tl  
  --TODO simplify
  --TODO remove allocated locs after exiting block
  BStmt (Block stmts) ->do{ 
      blockRes <- local id (evalBlock stmts);
      case blockRes of
        Nothing -> evalBlock tl
        Just finalVal -> return (Just finalVal) 
    }
    
  Decl type_ items ->do
    declCont <- declValueList (declIdents items) (declValueInit type_ items)
    declCont (evalBlock tl)

  DeclBlock type_ items -> throwError "Not implemented"
  Ass ident expr -> evalExpr expr >>= (\val-> modifyVariable ident (const val)) >> evalBlock tl 
  Incr ident -> (modifyVariable ident (\(NumVal n)-> NumVal $ n+1 ))  >> evalBlock tl
  Decr ident -> (modifyVariable ident (\(NumVal n)-> NumVal $ n-1 )) >> evalBlock tl
  Ret expr -> (evalExpr expr) >>= (return . Just)
  VRet -> return $ Just  VoidVal
  Print expr -> (evalExpr expr) >>= (\expr -> liftIO $ print expr ) >> (evalBlock tl)
  Cond expr stmt -> evalBlock ([CondElse expr stmt Empty]++tl)
  CondElse expr stmt1 stmt2 -> let evalAsBlock stmt =evalBlock ([BStmt (Block [stmt])]++tl) in
     (evalExpr expr) >>= (\ (BoolVal cond) -> if cond then evalAsBlock stmt1 else evalAsBlock stmt2 )
 
  --TODO test not sure 
  While expr stmt -> let whileLoop= (CondElse expr (BStmt $ Block [stmt,whileLoop]) Empty) in
    evalBlock ([whileLoop]++tl)
  SExp expr -> (evalExpr expr) >> evalBlock tl 

evalBlock [] = return Nothing

evalExpr :: Expr -> Result Value
evalExpr x = case x of
  EVar ident -> getValByIdent ident
  ELitInt integer -> return (NumVal integer)
  ELitTrue -> return (BoolVal True)
  ELitFalse -> return (BoolVal False)
  EApp ident args -> do
    let argRes= map evalExpr args
    (FunVal fun) <- getValByIdent ident
    evalFunction fun argRes 
  EString string -> return (StrVal string)
  Neg expr -> (evalExpr expr) >>=(\(NumVal v) -> return $ NumVal (-v) )
  Not expr -> (evalExpr expr) >>=(\(BoolVal v) -> return $ BoolVal (not v))

  EMul expr1 mulop expr2 -> do
    (NumVal v1)<- evalExpr expr1
    (NumVal v2)<- evalExpr expr2
    case mulop of {
    Times -> return $ NumVal (v1*v2);
    Div ->if v2==0 then throwError "cannot divide by 0" else return $ NumVal(v1 `quot` v2);
    Mod -> if v2 == 0 then throwError "cannot evaluate mod by 0" else return $ NumVal(v1 `mod` v2)
    } 

  EAdd expr1 addop expr2 -> evalArgs expr1 expr2 (\(NumVal v1) (NumVal v2) ->case addop of { 
    Plus -> NumVal(v1+v2);
    Minus -> NumVal(v1-v2)})

  ERel expr1 relop expr2 -> evalRelOp expr1 relop expr2
  EAnd expr1 expr2 -> evalArgs expr1 expr2 (\(BoolVal v1) (BoolVal v2) -> BoolVal(v1 && v2))
  EOr expr1 expr2 -> evalArgs expr1 expr2 (\(BoolVal v1) (BoolVal v2) -> BoolVal(v1 || v2))


evalArgs expr1 expr2 f  = do
  v1<- evalExpr expr1
  v2<- evalExpr expr2
  return $ f v1 v2

lessThan (BoolVal b1) (BoolVal b2) = b1 < b2 
lessThan (StrVal s1) (StrVal s2) = s1 < s2
lessThan (NumVal n1) (NumVal n2) = n1<n2  

equal (BoolVal b1) (BoolVal b2) = b1 == b2 
equal (StrVal s1) (StrVal s2) = s1 == s2
equal (NumVal n1) (NumVal n2) = n1 == n2  

evalRelOp :: Expr -> RelOp -> Expr -> Result Value
evalRelOp expr1 relop expr2 = do
  l <-evalExpr expr1
  r <-evalExpr expr2
  let res = case relop of{ 
    LTH ->(lessThan l r);
    LE -> (lessThan l r) || (equal l r);
    GTH -> lessThan r l;
    GE ->  (lessThan r l) || (equal l r);
    EQU -> equal l r;
    NE ->  not $ equal l r;
    } in return (BoolVal res)