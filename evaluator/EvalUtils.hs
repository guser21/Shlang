{-# LANGUAGE LambdaCase #-}

module EvalUtils where

import           AbsDeclaration

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
import           System.Exit            (exitFailure, exitSuccess)
import           System.IO

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
    _    -> throwError "functional or void types cannot be unintialized"

declVars :: Type -> [Ident] -> Result (Result a -> Result a)
declVars type_ nameIdents =
  declValueList nameIdents (map (\_ -> typeDefault type_) nameIdents)

declVar :: Type -> Ident -> Result (Result a -> Result a)
declVar type_ nameIdent = declValue nameIdent (typeDefault type_)

declValue :: Ident -> Result Value -> Result (Result a -> Result a)
declValue nameIdent resVal = do
  l <- newloc
  val <- resVal
  let nval =
        case val of
          FunVal fun env -> FunVal fun (Map.insert nameIdent l env)
          _              -> val
  modifyMem (Map.insert l nval)
  return (local (Map.insert nameIdent l))

declValueList :: [Ident] -> [Result Value] -> Result (Result a -> Result a)
declValueList (fn:nameIdents) (fv:values) = do
  declCont <- declValue fn fv
  declNextCont <- declValueList nameIdents values
  return $ declCont . declNextCont
declValueList [] [] = return (local id)
declValueList [] (h:t) = throwError "Mismatching argument list size"
declValueList (h:t) [] = throwError "Mismatching argument list size"

declIdents :: [Item] -> [Ident]
declIdents =
  map
    (\case
       NoInit ident -> ident
       Init ident expr -> ident)
