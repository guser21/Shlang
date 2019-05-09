module Utilities where

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
import           System.Exit            (exitFailure, exitSuccess)
import           System.IO

newloc :: Result Loc
newloc = do
  (st, l) <- get
  put (st, l + 1)
  return l

modifyMem :: (Mem -> Mem) -> Result ()
modifyMem f = modify (\(st, l) -> (f st, l))

getTypeByLoc :: Loc -> Result ValType
getTypeByLoc loc = do
  (st, l) <- get
  case Map.lookup loc st of
    Just val -> return val
    Nothing  -> throwError "cannot find location for name"

getTypeByIdent :: Ident -> Result ValType
getTypeByIdent var = do
  (env, _, _, _) <- ask
  (st, l) <- get
  let varLoc = Map.lookup var env
  case varLoc of
    Just loc -> getTypeByLoc loc
    Nothing  -> throwError $ "unknown indentificator " ++ show var

typeDefault :: Type -> Result ValType
typeDefault type_ =
  case type_ of
    Int  -> return $ SimpleType Int
    Bool -> return $ SimpleType Bool
    Str  -> return $ SimpleType Str
    _    -> throwError "unrecognized type"

getIdentFromItem :: Item -> Ident
getIdentFromItem (NoInit ident)    = ident
getIdentFromItem (Init ident expr) = ident

declValue :: Ident -> Result ValType -> Result (Result a -> Result a)
declValue nameIdent resVal = do
  (env, constName, shadowable, context) <- ask
  when
    (Set.notMember nameIdent shadowable && Map.member nameIdent env)
    (throwError $ "name " ++ show nameIdent ++ " is already in use")
  l <- newloc
  val <- resVal
  modifyMem (Map.insert l val)
  return
    (local
       (\(env, constNames, shadowVar, context) ->
          (Map.insert nameIdent l env, constNames, shadowVar, context)))

declValueTypeLists ::
     [(Type, [Ident])] -> Set.Set Ident -> Result (Result a -> Result a)
declValueTypeLists namesAndTypes newConstNames = do
  (env, constName, shadowable, context) <- ask
  let nameTypeFlat =
        concatMap (\(t, idents) -> map (\e -> (t, e)) idents) namesAndTypes
  let consts =
        foldl (\acc (t, ident) -> Set.delete ident acc) constName nameTypeFlat
  nenv <-
    foldl
      (\acc (type_, ident) -> do
         curEnv <- acc
         when
           (Set.notMember ident shadowable && Map.member ident curEnv)
           (throwError $ "name " ++ show ident ++ " is already in use")
         l <- newloc
         val <- typeDefault type_
         modifyMem (Map.insert l val)
         return (Map.insert ident l curEnv))
      (return env)
      nameTypeFlat
  return $
    local (const (nenv, consts `Set.union` newConstNames, shadowable, context))
