module TypeChecker where

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
import           ExprState
import           System.Exit            (exitFailure, exitSuccess)
import           System.IO
import           Utilities

mainFunc = Ident "main"

checkTypes :: Program -> Result ()
checkTypes (Program topDefs)
  --todo find a better way of doing this
 = do
  let globalDefs =
        foldl
          (\acc el ->
             case el of
               GlobDecl type_ items -> (type_, items) : acc
               GlobFinDecl type_ items -> (type_, items) : acc
               FnDef type_ ident args _ ->
                 (FuncType (argsToTypes args) type_, [NoInit ident]) : acc)
          []
          topDefs
  let globalFinalIdents =
        foldl
          (\acc el ->
             case el of
               GlobFinDecl type_ items -> map getIdentFromItem items ++ acc
               _                       -> acc)
          []
          topDefs
  let functions =
        filter
          (\e ->
             case e of
               FnDef reType ident args b -> True
               _                         -> False)
          topDefs
  (env, consts, _, _) <- ask
  let constIdents = foldl (flip Set.insert) consts globalFinalIdents
  traverse_
    (\(type_, items) ->
       checkDeclTypes type_ items >>=
       flip unless (throwError $ "incorrect declaration of type " ++ show type_))
    globalDefs
  let typeAndIdent =
        map (\(type_, items) -> (type_, map getIdentFromItem items)) globalDefs
  declCont <- declValueTypeLists typeAndIdent constIdents
  -- declCont (checkAllFunctions functions)
  traverse_ checkFunction functions

-- checkAllFunctions (h:tl) =
--   case h of
--     FnDef reType ident args block -> do
--       declCont <- declValue ident (FunType h)
--       declCont (checkAllFunctions tl)
--     _ -> throwError "unexpected type in fun match"
-- checkAllFunctions [] = do
--   (env, constName, _, _) <- ask
--   traverse_
--     (\(_, loc) ->
--        getTypeByLoc loc >>=
--        (\e ->
--           case e of
--             FunType f -> checkFunction f
--             _         -> return True))
--     (Map.toList env)
-- checkAllFunctions funs = traverse_ checkFunction
checkProgramTypesIO :: Program -> IO Bool
checkProgramTypesIO prog = do
  ans <-
    runExceptT
      (runStateT
         (runReaderT (checkTypes prog) (Map.empty, Set.empty, 0, OtherContext))
         (Map.empty, 0, 0, Set.empty))
  case ans of
    (Left errMesg) -> putStrLn ("Type error: " ++ errMesg) >> return False
    _              -> return True --ended as supposed
