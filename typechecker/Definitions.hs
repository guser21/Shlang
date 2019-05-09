module Definitions where

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
import           System.Exit            (exitFailure, exitSuccess)
import           System.IO

data ValType
  = SimpleType { t :: Type }
  | NoRetType
  | FunType { fun :: TopDef }
  deriving (Eq)

instance Show ValType where
  show v =
    case v of
      FunType _        -> "function"
      NoRetType        -> "no return type"
      SimpleType type_ -> show type_

data Context
  = WhileContext
  | OtherContext
  deriving (Eq, Show)

type Loc = Integer

type ConstIdent = Set.Set Ident

type OverShadowableIdent = Set.Set Ident

type Env = (Map.Map Ident Loc, ConstIdent, OverShadowableIdent, Context)

type Mem = Map.Map Loc ValType

type Store = (Mem, Loc)

type Result = ReaderT Env (StateT Store (ExceptT String IO))
