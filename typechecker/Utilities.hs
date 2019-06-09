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

makeNewBlock newBlockId (env, constIdent, curBlockId, context) =
  (env, constIdent, newBlockId, context)

getNewBlockId :: Result BlockId
getNewBlockId = do
  (st, l, curBlockId, identR) <- get
  put (st, l, curBlockId + 1, identR)
  return $ curBlockId + 1

getCurBlockId :: Result BlockId
getCurBlockId = do
  (st, l, curBlockId, _) <- get
  return curBlockId

newloc :: Result Loc
newloc = do
  (st, l, bi, identR) <- get
  put (st, l + 1, bi, identR)
  return l

modifyMem :: (Mem -> Mem) -> Result ()
modifyMem f = modify (\(st, l, mb, identR) -> (f st, l, mb, identR))

getTypeByLoc :: Loc -> Result ValType
getTypeByLoc loc = do
  (st, l, _, _) <- get
  case Map.lookup loc st of
    Just val -> return val
    Nothing  -> throwError "cannot find location for name"

getTypeByIdent :: Ident -> Result ValType
getTypeByIdent var = do
  (env, _, _, _) <- ask
  (st, l, _, _) <- get
  let varLoc = Map.lookup var env
  case varLoc of
    Just loc -> getTypeByLoc loc
    Nothing  -> throwError $ "unknown indentificator " ++ show var

typeDefault :: Type -> Result ValType
typeDefault type_ =
  case type_ of
    Int          -> return $ SimpleType Int
    Bool         -> return $ SimpleType Bool
    Str          -> return $ SimpleType Str
    FuncType _ _ -> return $ SimpleType type_
    _            -> throwError $ "unrecognized type of " ++ show type_

getIdentFromItem :: Item -> Ident
getIdentFromItem (NoInit ident)    = ident
getIdentFromItem (Init ident expr) = ident

declValue :: Ident -> ValType -> Result (Result a -> Result a)
declValue nameIdent val = do
  (env, constName, curBlockId, context) <- ask
  checkIdent nameIdent
  rememberIdent nameIdent
  l <- newloc
  modifyMem (Map.insert l val)
  return
    (local
       (\(env, constNames, curBlockId, context) ->
          (Map.insert nameIdent l env, constNames, curBlockId, context)))

declValueTypeLists ::
     [(Type, [Ident])] -> Set.Set Ident -> Result (Result a -> Result a)
declValueTypeLists namesAndTypes newConstNames = do
  (env, constName, curBlockId, context) <- ask
  let nameTypeFlat =
        concatMap (\(t, idents) -> map (\e -> (t, e)) idents) namesAndTypes
  let consts =
        foldl (\acc (t, ident) -> Set.delete ident acc) constName nameTypeFlat
  nenv <-
    foldl
      (\acc (type_, ident) -> do
         curEnv <- acc
         checkIdent ident
         rememberIdent ident
         l <- newloc
         val <- typeDefault type_
         modifyMem (Map.insert l val)
         return (Map.insert ident l curEnv))
      (return env)
      nameTypeFlat
  return $
    local (const (nenv, consts `Set.union` newConstNames, curBlockId, context))

rememberIdent :: Ident -> Result ()
rememberIdent ident = do
  (st, l, maxBlockId, identR) <- get
  (_, _, curBlockId, _) <- ask
  let nIdentR = Set.insert (ident, curBlockId) identR
  put (st, l, maxBlockId, nIdentR)

checkIdent :: Ident -> Result ()
checkIdent ident = do
  (st, l, maxBlockId, identR) <- get
  (_, _, curBlockId, _) <- ask
  when
    (Set.member (ident, curBlockId) identR)
    (throwError $ "name " ++ show ident ++ " is already in use")

fastEvalBool :: Expr -> Maybe Bool
fastEvalBool expr =
  case expr of
    ELitTrue  -> Just True
    ELitFalse -> Just False
    _         -> Nothing

argsToTypes = map (\(Arg argType _) -> argType)

argsToCompoundTypes = map (\(Arg argType _) -> SimpleType argType)

isPrintable :: ValType -> Result ()
isPrintable type_ =
  case type_ of
    SimpleType Int  -> return ()
    SimpleType Str  -> return ()
    SimpleType Bool -> return ()
    _               -> throwError $ "cannot print of type " ++ show type_
