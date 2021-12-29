{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MonoLocalBinds             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE PartialTypeSignatures      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{-# LANGUAGE QuasiQuotes, ExtendedDefaultRules #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE QuasiQuotes, ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}

module ErgoDex.Contracts.Pool where

import Plutus.V1.Ledger.Value
import qualified Data.Text as T
import qualified Prelude                          as Haskell
import           Ledger
import           Ledger.Constraints.OnChain       as Constraints
import           Ledger.Constraints.TxConstraints as Constraints
import           Ledger.Value                     (AssetClass (..), CurrencySymbol(..), symbols, assetClassValue, isZero, flattenValue, TokenName(..))
import           Playground.Contract              (FromJSON, Generic, ToJSON, ToSchema)
import           ErgoDex.Contracts.Types
import qualified PlutusTx
import           PlutusTx.Prelude
import qualified PlutusTx.Builtins     as BI
import           PlutusTx.IsData.Class
import           PlutusTx.Sqrt
import           Utils
import qualified PlutusTx.Builtins.Internal as BII
import qualified PlutusTx.AssocMap  as Map
import qualified PlutusTx.List as List
import qualified Prelude              as P
import  PlutusTx.Builtins.Class 
import PlutusTx.Maybe as Option
import Data.String.Interpolate ( i )
data PoolParams = PoolParams
  { poolNft :: Coin Nft
  , poolX   :: Coin X
  , poolY   :: Coin Y
  , poolLq  :: Coin Liquidity
  , feeNum  :: Integer
  } deriving (Haskell.Show, Generic, ToJSON, FromJSON, ToSchema)
PlutusTx.makeIsDataIndexed ''PoolParams [('PoolParams, 0)]
PlutusTx.makeLift ''PoolParams

instance Eq PoolParams where
  {-# INLINABLE (==) #-}
  x == y = poolNft x == poolNft y &&
           poolX x   == poolX y &&
           poolY x   == poolY y &&
           poolLq x  == poolLq y &&
           feeNum x  == feeNum y

data PoolDatum = PoolDatum PoolParams (Amount Liquidity)
  deriving stock (Haskell.Show)
PlutusTx.makeIsDataIndexed ''PoolDatum [('PoolDatum, 0)]
PlutusTx.makeLift ''PoolDatum

data PoolAction = Init | Deposit | Redeem | Swap
  deriving Haskell.Show
PlutusTx.makeIsDataIndexed ''PoolAction [ ('Init ,   0)
                                        , ('Deposit, 1)
                                        , ('Redeem,  2)
                                        , ('Swap,    3)
                                        ]
PlutusTx.makeLift ''PoolAction

data PoolState = PoolState
  { reservesX :: Amount X
  , reservesY :: Amount Y
  , liquidity :: Amount Liquidity
  } deriving Haskell.Show

{-# INLINABLE mkPoolState #-}
mkPoolState :: PoolParams -> Amount Liquidity -> TxOut -> PoolState
mkPoolState PoolParams{..} lq out =
    PoolState x y lq
  where
    value = txOutValue out
    x     = amountOf value poolX
    y     = amountOf value poolY

data PoolDiff = PoolDiff
  { diffX         :: Diff X
  , diffY         :: Diff Y
  , diffLiquidity :: Diff Liquidity
  }

{-# INLINABLE diffPoolState #-}
diffPoolState :: PoolState -> PoolState -> PoolDiff
diffPoolState s0 s1 =
    PoolDiff dx dy dlq
  where
    rx0 = unAmount $ reservesX s0
    rx1 = unAmount $ reservesX s1
    ry0 = unAmount $ reservesY s0
    ry1 = unAmount $ reservesY s1
    l0  = unAmount $ liquidity s0
    l1  = unAmount $ liquidity s1
    dx  = Diff $ rx1 - rx0
    dy  = Diff $ ry1 - ry0
    dlq = Diff $ l1 - l0

{-# INLINABLE getPoolOutput #-}
getPoolOutput :: ScriptContext -> TxOut
getPoolOutput ss@ScriptContext{scriptContextTxInfo=TxInfo{txInfoOutputs}} =
  head txInfoOutputs -- pool box is always 1st output

{-# INLINABLE getPoolOutputReversed #-}
getPoolOutputReversed :: ScriptContext -> TxOut
getPoolOutputReversed ss@ScriptContext{scriptContextTxInfo=TxInfo{txInfoOutputs}} =
  head $ reverse txInfoOutputs -- pool box is always 1st output

{-# INLINABLE getPoolInput #-}
getPoolInput :: ScriptContext -> TxOut
getPoolInput ScriptContext{scriptContextTxInfo=TxInfo{txInfoInputs}} =
  txInInfoResolved $ head txInfoInputs -- pool box is always 1st input

{-# INLINABLE findPoolDatum #-}
findPoolDatum :: TxInfo -> DatumHash -> (PoolParams, Amount Liquidity)
findPoolDatum info h = case findDatum h info of
  Just (Datum d) -> case fromBuiltinData d of
    (Just (PoolDatum ps lq)) -> (ps, lq)
    _                        -> traceError "error decoding pool data"
  _              -> traceError "pool input datum not found"

{-# INLINABLE validInit #-}
validInit :: PoolState -> PoolDiff -> Bool
validInit PoolState{..} PoolDiff{..} =
    traceIfFalse "Illegal initial pool state" validInitialState &&
    traceIfFalse "Illegal amount of liquidity forged" (diffLiquidity' <= liquidityUnlocked)
  where
    diffLiquidity' = unDiff diffLiquidity
    diffX'         = unDiff diffX
    diffY'         = unDiff diffY
    liquidity'     = unAmount liquidity
    reservesX'     = unAmount reservesX
    reservesY'     = unAmount reservesY

    validInitialState =
      liquidity' == 0 &&
      reservesX' == 0 &&
      reservesY' == 0

    liquidityUnlocked = case isqrt (diffX' * diffY') of
      Exactly l | l > 0       -> l
      Approximately l | l > 0 -> l + 1
      _                       -> traceError "insufficient liquidity"

{-# INLINABLE validDeposit #-}
validDeposit :: PoolState -> PoolDiff -> Bool
validDeposit PoolState{..} PoolDiff{..} =
    traceIfFalse "Illegal amount of liquidity forged" (diffLiquidity' <= liquidityUnlocked)
  where
    diffLiquidity' = unDiff diffLiquidity
    diffX'         = unDiff diffX
    diffY'         = unDiff diffY
    liquidity'     = unAmount liquidity
    reservesX'     = unAmount reservesX
    reservesY'     = unAmount reservesY

    liquidityUnlocked = min (divide (diffX' * liquidity') reservesX') (divide (diffY' * liquidity') reservesY')

{-# INLINABLE validRedeem #-}
validRedeem :: PoolState -> PoolDiff -> Bool
validRedeem PoolState{..} PoolDiff{..} =
    traceIfFalse "Illegal redeem" fairRedeem
  where
    diffLiquidity' = unDiff diffLiquidity
    diffX'         = unDiff diffX
    diffY'         = unDiff diffY
    liquidity'     = unAmount liquidity
    reservesX'     = unAmount reservesX
    reservesY'     = unAmount reservesY

    fairRedeem =
      diffX' * liquidity' >= diffLiquidity' * reservesX' && diffY' * liquidity' >= diffLiquidity' * reservesY'

{-# INLINABLE validSwap #-}
validSwap :: PoolParams -> PoolState -> PoolDiff -> Bool
validSwap PoolParams{..} PoolState{..} PoolDiff{..} =
    traceIfFalse "Illegal swap" fairSwap &&
    traceIfFalse "Liquidity emission must not change" (diffLiquidity == 0)
  where
    reservesX0     = unAmount reservesX
    reservesY0     = unAmount reservesY
    deltaReservesX = unDiff diffX
    deltaReservesY = unDiff diffY
    feeDen         = 1000

    fairSwap =
      if deltaReservesX > 0 then
        reservesY0 * deltaReservesX * feeNum >= negate deltaReservesY * (reservesX0 * feeDen + deltaReservesX * feeNum)
      else
        reservesX0 * deltaReservesY * feeNum >= negate deltaReservesX * (reservesY0 * feeDen + deltaReservesY * feeNum)

{-# INLINABLE getData #-}
getData :: Coin a -> BI.BuiltinString
getData Coin{..} = let name = unCurrencySymbol $ fst (unAssetClass unCoin) in BI.decodeUtf8 name

{-# INLINABLE getTokenName #-}
getTokenName :: Coin a -> BI.BuiltinString
getTokenName Coin{..} = let name = unTokenName $ snd (unAssetClass unCoin) in BI.decodeUtf8 name

-- {-# INLINABLE valueToList #-}
-- valueToList :: Value -> [(CurrencySymbol, Map.Map TokenName Integer)]
-- valueToList Value{..} = Map.toList getValue

-- iterateF1 :: [(CurrencySymbol, Map.Map TokenName Integer)] -> BI.BuiltinString
-- iterateF1 x:xs =
  
-- iterateC :: CurrencySymbol -> Map.Map TokenName Integer -> BI.BuiltinString
-- iterateC symbol tokenNames =
--   let
--     s = BI.decodeUtf8 $ unCurrencySymbol symbol
--     tk = foldWithKey (BI.appendString iterateT "//") BI.emptyString tokenNames


-- iterateT :: TokenName -> Integer -> BI.BuiltinString
-- iterateT tokenName i =
--   let
--     t = BI.decodeUtf8 $ unTokenName tokenName
--     i1 = BI.decodeUtf8 $ BI.unsafeDataAsB $ BI.mkI i
--   in
--     BI.appendString (BI.appendString t "..") i1

{-# INLINABLE valueToBS #-}
valueToBS :: Value -> BI.BuiltinString
valueToBS Value{..} =
  let
    listValue =  Map.toList $ getValue
  in
    List.foldr (\(k, v) acc -> BI.appendString (BI.appendString (BI.appendString (BI.appendString (mapValueToInt v) ".") ("unCurrencySymbol")) ".") acc) "" listValue

{-# INLINABLE mkSize #-}
mkSize :: Value -> BI.BuiltinString
mkSize Value{..} =
  let
    e = Map.keys $ getValue
  in List.foldr (\k acc -> BI.appendString ("1") acc) "1" e

{-# INLINABLE mkStrWithSep #-}
mkStrWithSep :: BI.BuiltinString -> BI.BuiltinString
mkStrWithSep input = BI.appendString input " :: "

{-# INLINABLE mapValueToInt #-}
mapValueToInt :: Map.Map TokenName Integer -> BI.BuiltinString
mapValueToInt input =
  let
    e = Map.toList input
  in
    List.foldr (\(k, v) acc -> BI.appendString (BI.appendString (BI.appendString (BI.appendString (BI.decodeUtf8 $ unTokenName k) ".") (getListSize k v)) "|" ) acc) "" e
-- (BI.decodeUtf8 $ unTokenName k)
{-# INLINABLE getListSize #-}
getListSize :: TokenName -> Integer -> BI.BuiltinString
getListSize tn input =
  let
    unTN = BI.decodeUtf8 $ unTokenName tn
  in BI.appendString (BI.appendString (BI.appendString "(" unTN) (if (input > 0) then "InputT>0" else "IntputT<0")) ")"

{-# INLINABLE mkIntegerToBuiltinString #-}
mkIntegerToBuiltinString :: Integer -> BI.BuiltinString
mkIntegerToBuiltinString i =
  Haskell.undefined

{-# INLINABLE testFunc #-}
testFunc :: Value -> BI.BuiltinString
testFunc v =
  BI.decodeUtf8 $ BI.unsafeDataAsB $ toBuiltinData v

{-# INLINABLE mkOutSize #-}
mkOutSize :: PoolDatum -> ScriptContext -> BI.BuiltinString
mkOutSize (PoolDatum ps0@PoolParams{..} lq0) ScriptContext{scriptContextTxInfo=TxInfo{..}} =
  let
    inS = List.foldr (\k acc -> BI.appendString "Input" acc) "" txInfoInputs
    outS = List.foldr (\k acc -> BI.appendString "Output" acc) "" txInfoOutputs
    checkPoolNft = List.foldr (\k acc -> BI.appendString (if (isUnit (txOutValue k) poolNft) then "Equal" else "NotEqual") acc) "" txInfoOutputs
    allValues = List.foldr (\k acc -> BI.appendString (BI.appendString (valueToBS $ txOutValue k) ".") acc) "" txInfoOutputs
    -- checkCurrSymbol = 
    --   List.foldr 
    --     (\k acc -> 
    --       List.foldr 
    --         (\k1 acc -> BI.appendString (if (BI.equalsByteString k1 "805fe1efcdea11f1e959eff4f422f118aa76dca2d0d797d184e487da") then "csiseq!" else "csisnoteq!") acc) "" (List.map (\cs -> unCurrencySymbol cs) (Map.keys $ getValue $ txOutValue k))
    --       )
    --     "" 
    --     txInfoOutputs
    checkLength =
      List.foldr 
        (\k acc -> 
          List.foldr 
            (\k1 acc -> BI.appendString (if ((BI.lengthOfByteString k1) > 0) then "lenght>0!" else "length<=0!") acc) "" (List.map (\cs -> unCurrencySymbol cs) (Map.keys $ getValue $ txOutValue k))
          )
        "" 
        txInfoOutputs

    checkValueLength =
      BI.appendString (BI.appendString "(" (List.foldr
        (\k acc ->
          List.foldr (\k1 acc -> BI.appendString "(+1CS)" acc) "" (Map.keys $ getValue $ txOutValue k)
        ) "" txInfoOutputs)) ")"
  in 
    -- BI.appendString (BI.appendString ( 
      -- BI.appendString (BI.appendString (
        BI.appendString (BI.appendString (BI.appendString (BI.appendString (BI.appendString (BI.appendString (BI.appendString inS ".") outS) ".") checkPoolNft) ".") allValues) checkValueLength
        --  ".") checkCurrSymbol
    --  ) "." ) checkLength
  
{-# INLINABLE mkPoolValidator #-}
mkPoolValidator :: PoolDatum -> PoolAction -> ScriptContext -> Bool
mkPoolValidator pd@(PoolDatum ps0@PoolParams{..} lq0) action ctx =
    -- traceIfFalse 
    --   (BI.appendString 
    --   (BI.appendString
    --     (BI.appendString 
    --       (BI.appendString (BI.appendString (BI.appendString (BI.appendString "Pool NFT not preserved. " (getData poolNft)) ".") (getTokenName poolNft)) " qwerty12345678") (mkOutSize pd ctx)
    --     ) "qwerty12345678"
    --   ) (mkSize $ txOutValue successor)) poolNftPreserved &&
    traceIfFalse "Dummy" boolRes &&
    traceIfFalse "Pool params not preserved" poolParamsPreserved &&
    traceIfFalse "Illegal amount of liquidity declared" liquiditySynced &&
    traceIfFalse "Assets qty not preserved" strictAssets &&
    traceIfFalse "Script not preserved" scriptPreserved &&
    traceIfFalse "Invalid action" validAction
  where
    txInfo    = scriptContextTxInfo ctx
    self      = getPoolInput ctx
    successor = getPoolOutput ctx
    successorReversed = getPoolOutputReversed ctx

    poolNftPreserved = isUnit (txOutValue successor) poolNft

    getDatumHash1 = Option.maybe "EmptyDatumHash1" (\(DatumHash a) -> BI.decodeUtf8 a) (txOutDatum successor)
    getDatumHash2 = Option.maybe "EmptyDatumHash2" (\(DatumHash a) -> BI.decodeUtf8 a) (txOutDatum successorReversed)

    g1 = BI.appendString (BI.appendString "(" getDatumHash1) ")"
    g2 = BI.appendString (BI.appendString "(" getDatumHash2) ")"

    g3 = BI.appendString g1 g2

    r = traceError (BI.appendString "DatumHashes are:" g3)

    boolRes = r == BI.emptyString

    (ps1, lq1) = case txOutDatum successor of
      Nothing -> traceError "pool output datum hash not found"
      Just h  -> findPoolDatum txInfo h

    poolParamsPreserved = ps1 == ps0

    s0   = mkPoolState ps0 lq0 self
    s1   = mkPoolState ps1 lq1 successor
    diff = diffPoolState s0 s1

    valueMinted = txInfoMint txInfo

    liquiditySynced = isZero valueMinted ||
                      valueMinted == (assetClassValue (unCoin poolLq) (unDiff $ diffLiquidity diff))

    numAssets    = length $ flattenValue (txOutValue successor)
    strictAssets = numAssets == 3

    scriptPreserved = (txOutAddress successor) == (txOutAddress self)

    validAction = case action of
      Init    -> validInit s0 diff
      Deposit -> validDeposit s0 diff
      Redeem  -> validRedeem s0 diff
      Swap    -> validSwap ps0 s0 diff
