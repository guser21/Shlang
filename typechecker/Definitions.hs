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
  deriving (Eq)

instance Show ValType where
  show v =
    case v of
      NoRetType        -> "no return type"
      SimpleType type_ -> show type_

data Context
  = WhileContext
  | OtherContext
  deriving (Eq, Show)

type Loc = Integer

type MaxBlockId = Integer

type CurBlockId = Integer

type BlockId = Integer

type ConstIdent = Set.Set Ident

type OverShadowableIdent = Set.Set Ident

type Env = (Map.Map Ident Loc, ConstIdent, CurBlockId, Context)

type Mem = Map.Map Loc ValType

type Store = (Mem, Loc, MaxBlockId,Set.Set (Ident,BlockId))

type Result = ReaderT Env (StateT Store (ExceptT String IO))
