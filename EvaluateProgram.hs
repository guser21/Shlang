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

data Value = BoolVal {bool :: Bool} | NumVal {num :: Integer} | StrVal {str :: String} | FunVal {fun :: TopDef } deriving (Eq, Show)  

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

mainFunc =Ident "main"


failure :: Show a => a -> Result ()
failure x = liftIO $ print ("Undefined case: " ++ show x)

transIdent :: Ident -> Result ()
transIdent x = case x of
  Ident string -> failure x
  

runProgram ::Program -> Result ()
runProgram (Program topDefs) = runFunctions topDefs


runFunctions :: [TopDef] -> Result ()
runFunctions (h:tl) = do 
  let FnDef reType ident args block = h 
  liftIO $ print ident;
  funLoc <- newloc;
  modifyMem (Map.insert funLoc (FunVal h))
  local (Map.insert ident funLoc) (runFunctions tl)

runFunctions [] = do
  env <- ask;
  case  (Map.lookup  mainFunc env) of
    Just val -> liftIO $ print val
    Nothing -> throwError "RuntimeError: Cannot find definition of function main"


runProgramIO :: Program-> IO ()
runProgramIO prog = do
   ans <- runExceptT (runStateT (runReaderT (runProgram prog) Map.empty) (Map.empty,0)) ; 
   case ans of 
    (Left errMesg) -> putStrLn $ "Runtime error: " ++ errMesg
    _ -> print ans


