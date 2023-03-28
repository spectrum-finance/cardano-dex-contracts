# Spectrum AMM DEX on Cardano

Spectrum AMM protocol is built on top of 2 essential kinds of on-chain entities:
 - Pool
 - Proxy Order

 Each entity is represented on-chain as a UTxO guarded with a specific validator.
 Now we are going to describe entities of each type in details.

 ## Pool

 Pool represents AMM Pool that holds reserves of token pair, plus two technical tokens:
 - PoolID - NFT allowing to uniquly identify pool UTxO. PoolID is supposed to remain in the pool for the whole lifetime of the pool.
 - LQ - token that represents the share of a Liquidity Provider in the pool. LQ token is used to redeem liqudity.

To describe the life-cycle of the Pool we use the notion of Persistent Entity, meaning that Pool entity is reproduced again and again with each operation applied to it. State Transition Function of Pool can be described as `Apply: S -> O -> Maybe S'`, where `S` initial pool state, `O` - operation of type `Deposit|Redeem|Swap|Destroy`, `S'` - resulting pool state.

Pool is guarded with a Pool Validator (`PContracts/PPool.hs`). The main point of this validator is to ensure all operations are done fairly relatively to the pool itself.

Specific checks that Pool Validator performs are annotated in `PContracts/PPool.hs`.

## Orders

Orders hold operation inputs delegated to them by DEX users. 
In contrast to Pool Order is an Emphemeral Entity and is eliminated when applied to pool or redeemed.
Each order is guarded with a corresponding type of validator which main task is to ensure the relevant operation is performed properly from user's viewpoint (in contrast to Pool Validator which only checks that pool is not "fooled").

Below we discuss each order type in detail.

### Swap

Swap order holds swap inputs - base asset (asset being exchanged) and executor fee (currently in ADA).
Swap validator ensures that user receives sufficient amount of quote tokens in exchange for base token and pays fair amount of fee to executor.

Specific checks that Swap Validator performs are annotated in `PContracts/PSwap.hs`.

### Deposit

Deposit order holds deposit inputs - a pair of assets and executor fee (currently in ADA).
Deposit validator ensures that user receives sufficient amount of LQ tokens in exchange for pool pair tokens and pays fair amount of fee to executor.

Specific checks that Deposit Validator performs are annotated in `PContracts/PDeposit.hs`.

### Redeem

Redeem order holds redeem inputs - LQ tokens and executor fee (currently in ADA).
Redeem validator ensures that user receives sufficient amount of pool pair tokens in exchange for LQ tokens and pays fair amount of fee to executor.

Specific checks that Deposit Validator performs are annotated in `PContracts/PDeposit.hs`.
