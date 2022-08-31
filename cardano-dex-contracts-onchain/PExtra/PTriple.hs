module PExtra.PTriple where

import Plutarch.Builtin (pconstrBuiltin, pforgetData)
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

type PTuple3 a b c =
    PDataSum
        '[ '[ "_0" ':= a
            , "_1" ':= b
            , "_2" ':= c
            ]
         ]

ptuple3 :: Term s (PAsData a :--> PAsData b :--> PAsData c :--> PTuple3 a b c)
ptuple3 = phoistAcyclic $
    plam $ \x y z ->
        let target :: Term _ (PAsData (PBuiltinPair PInteger (PBuiltinList PData)))
            target = pconstrBuiltin # 0 #$ pcons # pforgetData x #$ pcons # pforgetData y #$ pcons # pforgetData z # pnil
         in punsafeCoerce target
