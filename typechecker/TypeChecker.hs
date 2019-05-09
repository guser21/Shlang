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
import           Expressions
import           Statements
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
               GlobDecl type_ items    -> (type_, items) : acc
               GlobFinDecl type_ items -> (type_, items) : acc
               _                       -> acc)
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
  (env, consts, shadow, _) <- ask
  let constIdents = foldl (flip Set.insert) consts globalFinalIdents
  traverse_
    (\(type_, items) ->
       checkDeclTypes type_ items >>=
       flip unless (throwError $ "incorrect declaration of type " ++ show type_))
    globalDefs
  let typeAndIdent =
        map (\(type_, items) -> (type_, map getIdentFromItem items)) globalDefs
  declCont <- declValueTypeLists typeAndIdent constIdents
  declCont (checkAllFunctions functions)

checkAllFunctions (h:tl) =
  case h of
    FnDef reType ident args block -> do
      declCont <- declValue ident (return $ FunType h)
      declCont (checkAllFunctions tl)
    _ -> throwError "unexpected type in fun match"
checkAllFunctions [] = do
  (env, constName, _, _) <- ask
  traverse_
    (\(_, loc) ->
       getTypeByLoc loc >>=
       (\e ->
          case e of
            FunType f -> checkFunction f
            _         -> return True))
    (Map.toList env)

checkFunction :: TopDef -> Result Bool
checkFunction (FnDef retType ident argDefs block) = do
  let argDecl =
        map (\(Arg argType argIdent) -> Decl argType [NoInit argIdent]) argDefs
  let Block stmts = block
  btype <- getBlockType (Block $ argDecl ++ stmts) (SimpleType retType)
  if btype == SimpleType retType || (btype == NoRetType && retType == Void)
    then return True
    else throwError $ "function " ++ show ident ++ " has a wrong return type"

checkProgramTypesIO :: Program -> IO Bool
checkProgramTypesIO prog = do
  ans <-
    runExceptT
      (runStateT
         (runReaderT
            (checkTypes prog)
            (Map.empty, Set.empty, Set.empty, OtherContext))
         (Map.empty, 0))
  case ans of
    (Left errMesg) -> putStrLn ("Type error: " ++ errMesg) >> return False
    _              -> return True --ended as supposed
