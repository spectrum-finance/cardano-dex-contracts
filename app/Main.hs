module Main where

import ErgoDex.Contracts.Proxy.PlutarchSwap
import Plutarch 
import RIO
import ErgoDex.Contracts.Proxy.PUtils
import Data.Text (Text)
import Plutarch.Evaluate (evaluateScript)
import Plutarch (ClosedTerm, compile)
import Plutus.V1.Ledger.Api (ExBudget)
import Plutus.V1.Ledger.Scripts (Script (unScript), ScriptError, applyArguments)
import UntypedPlutusCore (DeBruijn, DefaultFun, DefaultUni, Program)
import PlutusTx (Data)

main :: IO ()
main = do
    void $ print $ printTerm (pMkSwapValidator)
    --void $ print $ eval (pMkSwapValidator # pPSwapConfig # pPSwapRedeemer # ctx )