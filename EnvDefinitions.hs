module Evaluator where
import           Control.Monad.Identity
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Maybe
import qualified Data.Map                      as Map


data Value = BoolVal {bool :: Bool} | NumVal {num :: Integer} | StrVal {str :: String} deriving (Eq, Show)  

type Name = String
type Loc = Integer

type Env = Map.Map Name Loc
type Mem = Map.Map Loc Value

type Store = (Mem,Loc)

type Result a = ReaderT Env (ExceptT String (WriterT [String] (StateT Store IO))) a



runEval env st ev = runStateT (runWriterT (runExceptT (runReaderT (eval7 ev) env))) st

