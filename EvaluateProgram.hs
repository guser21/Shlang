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
   deriving (Eq, Show)  

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

declValue :: Ident -> Value -> Result (Result a -> Result a)
declValue nameIdent val =  do
   l <- newloc;
   modifyMem (Map.insert l val);
   return (local (Map.insert nameIdent l))

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
  case  (Map.lookup  mainFunc env) of
    Just val -> liftIO $ print val -- TODO
    Nothing -> throwError "Cannot find definition of function main"


runProgramIO :: Program-> IO ()
runProgramIO prog = do
   ans <- runExceptT (runStateT (runReaderT (runProgram prog) Map.empty) (Map.empty,0)) ; 
   case ans of 
    (Left errMesg) -> putStrLn $ "Runtime error: " ++ errMesg
    _ -> print ans



-- --Todo typecheckint
evalFunction :: TopDef -> [Value] -> Result Value
evalFunction (FnDef funType funName argDefs block) argVals  = do
  let argIdent =  map (\(Arg argType argIdent)-> argIdent) argDefs 
  let Block stmts = block
  funArgDeclCont <- declValueList argIdent argVals
  resVal<-funArgDeclCont (evalBlock stmts)
  case resVal of 
    Nothing -> throwError $ "No return statemant in " ++ (show funName)   
    Just val -> return val 
  
--what if returns from an inside block
-- {
--   {
--     return 5
--   }
-- }
evalBlock :: [Stmt] -> Result (Maybe Value)
evalBlock _ =return $ Just (NumVal 1)
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
