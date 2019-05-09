module EvalDefs where

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
import           System.Exit            (exitFailure, exitSuccess)
import           System.IO

data Value
  = BoolVal { bool :: Bool }
  | NumVal { num :: Integer }
  | StrVal { str :: String }
  | FunVal { fun :: TopDef }
  | VoidVal
  | BreakVal
  | ContVal
  deriving (Eq)

instance Show Value where
  show v =
    case v of
      BoolVal b -> show b
      NumVal n  -> show n
      StrVal s  -> s
      FunVal f  -> show f
      BreakVal  -> "breakVal"
      ContVal   -> "contVal "
      VoidVal   -> "voidVal"

type Loc = Integer

-- Ident-> loc, break,continue
type Env = Map.Map Ident Loc

type Mem = Map.Map Loc Value

type StackCount = Integer

type Store = (Mem, Loc, StackCount)

type Result = ReaderT Env (StateT Store (ExceptT String IO))
