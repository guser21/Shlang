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


declValue :: Ident -> Value -> Result (Result a -> Result a)
declValue nameIdent val =  do
   l <- newloc;
   modifyMem (Map.insert l val);
   return (local (Map.insert nameIdent l))

declVar :: Type -> Ident-> Result (Result a -> Result a)
declVar varType nameIdent = do
  case varType of
    Int -> declValue nameIdent (NumVal 0)
    Bool -> declValue nameIdent (BoolVal True) 
    Str -> declValue nameIdent (StrVal "") 
    _ -> throwError "unrecognized type"    


declVars :: Type -> [Ident]-> Result (Result a -> Result a)
declVars varType nameIdents = do
  case varType of
    Int -> declValueList nameIdents (map (\_-> (NumVal 0)) nameIdents)
    Bool -> declValueList nameIdents (map (\_-> (BoolVal True))  nameIdents) 
    Str -> declValueList nameIdents (map (\_-> (StrVal ""))  nameIdents) 
    _ -> throwError "unrecognized type" 

declValueList :: [Ident] -> [Value] -> Result (Result a -> Result a)
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
  liftIO $ print ident;
  declCont <- declValue ident (FunVal h)
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
evalFunction :: TopDef -> [Value] -> Result Value
evalFunction (FnDef funType funName argDefs block) argVals  = do
  let argIdent =  map (\(Arg argType argIdent)-> argIdent) argDefs 
  let Block stmts = block
  funArgDeclCont <- declValueList argIdent argVals
  resVal <- funArgDeclCont (evalBlock stmts)
  case resVal of 
    Nothing -> throwError $ "No return statemant in " ++ (show funName)   
    Just val -> return val 
  
--what if returns from an inside block
-- {
--   {
--     return 5
--   }
-- }

-- evalBlock stmts =do
--   -- liftIO $ print bl 
--   return $ Just (NumVal 1)
--todo Maybe is not the way to go
-- {-


evalBlock :: [Stmt] -> Result (Maybe Value)
evalBlock (h:tl) = case h of
  Empty -> evalBlock tl  
  BStmt (Block stmts) -> do{ 
    blockRes <- local id (evalBlock stmts);
    case blockRes of
      Nothing -> evalBlock tl
      Just finalVal -> return (Just finalVal) 
  }

  Decl type_ items -> throwError "not implemented" 
    -- do
    -- declCont<- (foldl (\acc el -> case el of 
    --   NoInit ident-> do
    --     curDecl <- declVar type_ ident
    --     acc >>=(\fun-> return $ fun . curDecl) 
    --   Init ident expr -> do
    --     exprVal <- evalExpr expr
    --     curDecl <- declValue ident exprVal
    --     acc >>=(\fun-> return $ fun . curDecl)) (return id) items) 
    -- declCont (evalBlock tl)
       
  DeclBlock type_ items -> throwError "Not implemented"
  Ass ident expr -> evalExpr expr >>= (\val-> modifyVariable ident (const val)) >> evalBlock tl 
  Incr ident -> (modifyVariable ident (\(NumVal n)-> NumVal $ n+1 ))  >> evalBlock tl
  Decr ident -> (modifyVariable ident (\(NumVal n)-> NumVal $ n+1 )) >> evalBlock tl
  Ret expr -> (evalExpr expr) >>= (return . Just)
  VRet -> return $ Just  VoidVal
  Print expr -> (evalExpr expr) >>= (\expr -> liftIO $ print expr ) >> (evalBlock tl)

  --what if we return inside if statement
  Cond expr stmt ->throwError "Not implemented"  
  CondElse expr stmt1 stmt2 -> throwError "Not implemented" 
  While expr stmt -> throwError "Not implemented" 
  SExp expr -> (evalExpr expr) >> evalBlock tl 

evalBlock [] = return Nothing

evalExpr :: Expr -> Result Value
evalExpr x = case x of
  EVar ident -> getValByIdent ident
  ELitInt integer -> return (NumVal integer)
  ELitTrue -> return (BoolVal True)
  ELitFalse -> return (BoolVal False)
  EApp ident args -> throwError "Not implemented" 
    --  do
    -- let evArgs = map (\exp -> let Result arg = evalExpr exp in arg) exprs
    -- getValByIdent ident >>= (\ (FunVal fun) -> evalFunction fun evArgs)

  EString string -> return (StrVal string)
  Neg expr -> (evalExpr expr) >>=(\(NumVal v) -> return $ NumVal (-v) )
  Not expr -> (evalExpr expr) >>=(\(BoolVal v) -> return $ BoolVal (not v))
  EMul expr1 mulop expr2 -> do
    (NumVal v1)<- evalExpr expr1
    (NumVal v2)<- evalExpr expr2
    return (NumVal (v1*v2))
  EAdd expr1 addop expr2 -> do
    (NumVal v1)<- evalExpr expr1
    (NumVal v2)<- evalExpr expr2
    return (NumVal (v1+v2))
  ERel expr1 relop expr2 -> throwError "Not Implemented"
  EAnd expr1 expr2 -> do
    (BoolVal v1)<- evalExpr expr1
    (BoolVal v2)<- evalExpr expr2
    return (BoolVal (v1 && v2))
  EOr expr1 expr2 -> do
    (BoolVal v1)<- evalExpr expr1
    (BoolVal v2)<- evalExpr expr2
    return (BoolVal (v1 || v2))

-- evalBlock (h:t) = case h of
--   VRet -> return $ Just Void

--   Ret expr -> ( (evalExpr expr) >>= (\x ->Just x))
--   _ -> return $ (evalStmt h) >>= evalBlock t 
-- evalBlock [] = return Nothing  


-- evalStmt stmt = liftIO $ print (stmt) 

-- evalExpr :: Expr -> Result Value
-- evalExpr expr = do
--   liftIO $ print (expr)
--   return $ StrVal "exprEval"
-- -}