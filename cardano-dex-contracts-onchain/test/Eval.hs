{-# LANGUAGE RankNTypes          #-}
module Eval where

import Plutarch.Prelude
import PExtra.API
import Data.Text (Text, pack)
import Plutarch.Evaluate (evalScript, EvalError, evalTerm)
import Plutarch (ClosedTerm, compile, Config(..), TracingMode (..))
import PlutusLedgerApi.V1 (Data, ExBudget)
import PlutusLedgerApi.V1.Scripts (Script (unScript), applyArguments)
import Control.Arrow
import UntypedPlutusCore (DeBruijn, DefaultFun, DefaultUni, Program)
import PlutusTx (Data)
import Debug.Trace

evalConfig :: Config
evalConfig = Config NoTracing

evalWithArgs :: ClosedTerm a -> [Data] -> Either Text (ExBudget, [Text], Program DeBruijn DefaultUni DefaultFun ())
evalWithArgs x args = do
  cmp <- compile evalConfig x
  let (escr, budg, trc) = evalScript $ applyArguments cmp args
  scr <- left (pack . show) escr
  pure (budg, trc, unScript scr)

evalWithArgsT :: ClosedTerm a -> [Data] -> Either Text (Program DeBruijn DefaultUni DefaultFun ())
evalWithArgsT x args = do
  cmp <- compile evalConfig x
  let (escr, budg, trc) = evalScript $ applyArguments cmp args
  scr <- left (pack . show) escr
  pure (unScript scr)