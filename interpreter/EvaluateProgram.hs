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
import           EvalDefs
import           EvalStmt
import           EvalUtils
import           System.Exit            (exitFailure, exitSuccess)
import           System.IO


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
