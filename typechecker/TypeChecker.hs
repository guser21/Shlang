{-# LANGUAGE LambdaCase #-}

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

extractGlobalDefs :: [TopDef] -> [(Type, [Item])]
extractGlobalDefs =
  foldl
    (\acc el ->
       case el of
         GlobDecl type_ items -> (type_, items) : acc
         GlobFinDecl type_ items -> (type_, items) : acc
         FnDef type_ ident args _ ->
           (FuncType (argsToTypes args) type_, [NoInit ident]) : acc)
    []

extractFinalIdents :: [TopDef] -> [Ident]
extractFinalIdents =
  foldl
    (\acc el ->
       case el of
         GlobFinDecl type_ items -> map getIdentFromItem items ++ acc
         _                       -> acc)
    []

extractFunctions :: [TopDef] -> [TopDef]
extractFunctions =
  filter
    (\case
       FnDef reType ident args b -> True
       _ -> False)

checkTypes :: Program -> Result ()
checkTypes (Program topDefs) =
  do let globalDefs = extractGlobalDefs topDefs
     let globalFinalIdents = extractFinalIdents topDefs
     let functions = extractFunctions topDefs
     let typeAndIdent =
           map
             (\(type_, items) -> (type_, map getIdentFromItem items))
             globalDefs
     declCont <-
       declValueTypeLists typeAndIdent (Set.fromList globalFinalIdents)
     declCont (traverse_ (uncurry checkDeclTypes) globalDefs)
     declCont (traverse_ checkFunction functions)
     `catchError` (\e -> throwError $ "In global scope : " ++ e)

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
