module Main where

import Plutarch
import RIO
import PExtra.PUtils
import ErgoDex.PContracts.PPool
import Data.Text (Text)
import Plutarch.Evaluate (evaluateScript)
import Plutarch (ClosedTerm, compile)
import Plutus.V1.Ledger.Api (ExBudget)
import Plutus.V1.Ledger.Scripts (Script (unScript), ScriptError, applyArguments)
import UntypedPlutusCore (DeBruijn, DefaultFun, DefaultUni, Program)
import PlutusTx (Data)

main :: IO ()
main = do
    --void $ print $ printTerm (pMkSwapValidator)
    _ <- print $ eval (poolValidator # pPPoolConfig # pPPoolSwapRedeemer # ctx)
    _ <- print $ eval (poolValidator # pPPoolConfig # pPPoolDepositRedeemer # ctx)
    _ <- print $ eval (poolValidator # pPPoolConfig # pPPoolRedeemRedeemer # ctx)
    pure $ ()