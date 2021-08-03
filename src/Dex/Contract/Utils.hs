module Dex.Contract.Utils where

import           Plutus.V1.Ledger.Address
import           Plutus.V1.Ledger.Value
import           Plutus.V1.Ledger.TxId
import           Plutus.V1.Ledger.Scripts
import qualified PlutusTx
import           Ledger
import           Dex.Contract.Models (dexInstance)

{-# INLINABLE dexValidator #-}
dexValidator :: Validator
dexValidator = Scripts.validatorScript dexInstance

{-# INLINABLE dexContractHash #-}
dexContractHash :: ValidatorHash
dexContractHash = Scripts.validatorHash dexInstance

{-# INLINABLE inputLockedByDex #-}
inputLockedByDex :: ScriptContext -> TxOut
inputLockedByDex sCtx = case [ input
                               | input <- txInfoInputs (scriptContextTxInfo sCtx)
                               , txOutAddress (txInInfoResolved input) == scriptHashAddress dexContractHash
                               ] of
                          [o] -> txInInfoResolved o
                          _   -> traceError "expected exactly one output with contract hash"