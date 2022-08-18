{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}

module ErgoDex.Contracts.Types where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

import ErgoDex.Plutus (adaAssetClass)
import qualified Data.Text as Text
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON
import Plutus.V1.Ledger.Value (AssetClass (..), Value (..), assetClassValue, assetClassValueOf, TokenName(..), CurrencySymbol(..))
import qualified Data.ByteString.Base16  as Base16
import qualified Data.ByteString as BSS
import qualified Data.Text.Encoding  as TE
import Control.Monad ((>=>))
import qualified PlutusTx
import PlutusTx.Prelude
import Text.Printf (PrintfArg)
import qualified Prelude as Haskell

data Lovelace = Lovelace deriving (Haskell.Show, Haskell.Eq, Generic)

-- Pool NFT
data Nft = Nft deriving (Haskell.Show, Haskell.Eq, Generic)

-- First asset of a pool
data X = X deriving (Haskell.Show, Haskell.Eq, Generic)

-- Second asset of a pool
data Y = Y deriving (Haskell.Show, Haskell.Eq, Generic)

-- Liquidity token of a pool
data Liquidity = Liquidity deriving (Haskell.Show, Haskell.Eq, Generic)

-- First asset of a pool
data Quote = Quote deriving (Haskell.Show, Haskell.Eq, Generic)

-- Second asset of a pool
data Base = Base deriving (Haskell.Show, Haskell.Eq, Generic)

encodeByteString :: BSS.ByteString -> Text.Text
encodeByteString = TE.decodeUtf8 . Base16.encode

tokenName :: BSS.ByteString -> TokenName
tokenName = TokenName . toBuiltin

fromTokenName :: (BSS.ByteString -> r) -> (Text.Text -> r) -> TokenName -> r
fromTokenName handleBytestring handleText (TokenName bs) = either (\_ -> handleBytestring $ fromBuiltin bs) handleText $ TE.decodeUtf8' (fromBuiltin bs)

--todo: move to aeson utils
decodeByteString :: JSON.Value -> JSON.Parser BSS.ByteString
decodeByteString = JSON.withText "bytestring" (either Haskell.fail Haskell.pure . tryDecode)

asBase16 :: BSS.ByteString -> Text.Text
asBase16 bs = Text.concat ["0x", encodeByteString bs]

fromText :: Text.Text -> TokenName
fromText = tokenName . TE.encodeUtf8

tryDecode :: Text.Text -> Either Haskell.String BSS.ByteString
tryDecode = Base16.decode . TE.encodeUtf8

instance ToJSON CurrencySymbol where
  toJSON currencySymbol =
    JSON.object
      [ ( "unCurrencySymbol"
        , JSON.String .
          encodeByteString .
          fromBuiltin .
          unCurrencySymbol $
          currencySymbol)
      ]

instance FromJSON CurrencySymbol where
  parseJSON =
    JSON.withObject "CurrencySymbol" $ \object -> do
      raw   <- object JSON..: "unCurrencySymbol"
      bytes <- decodeByteString raw
      Haskell.pure $ CurrencySymbol $ toBuiltin bytes

instance ToJSON TokenName where
    toJSON = JSON.object . Haskell.pure . (,) "unTokenName" . JSON.toJSON .
        fromTokenName
            (\bs -> Text.cons '\NUL' (asBase16 bs))
            (\t -> case Text.take 1 t of "\NUL" -> Text.concat ["\NUL\NUL", t]; _ -> t)

instance FromJSON TokenName where
    parseJSON =
        JSON.withObject "TokenName" $ \object -> do
        raw <- object JSON..: "unTokenName"
        fromJSONText raw
        where
            fromJSONText t = case Text.take 3 t of
                "\NUL0x"       -> either Haskell.fail (Haskell.pure . tokenName) . tryDecode . Text.drop 3 $ t
                "\NUL\NUL\NUL" -> Haskell.pure . fromText . Text.drop 2 $ t
                _              -> Haskell.pure . fromText $ t

deriving instance ToJSON AssetClass
deriving instance FromJSON AssetClass

-- Type to distinguish tokens within a pool
newtype Coin a = Coin {unCoin :: AssetClass}
    deriving stock (Haskell.Show, Generic)
    deriving newtype (ToJSON, FromJSON, Haskell.Eq, Haskell.Ord)
    deriving newtype (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)

{-# INLINEABLE retagCoin #-}
retagCoin :: forall a b. Coin a -> Coin b
retagCoin (Coin ac) = Coin ac

{-# INLINEABLE valueOf #-}
valueOf :: Value -> Coin a -> Integer
valueOf v = assetClassValueOf v . unCoin

-- Difference of a token amount
newtype Diff a = Diff {unDiff :: Integer}
    deriving stock (Haskell.Show, Generic)
    deriving newtype (ToJSON, FromJSON, Eq, Ord, PrintfArg)
    deriving newtype (Haskell.Eq, Haskell.Ord, Haskell.Num)
    deriving newtype (AdditiveGroup, AdditiveMonoid, AdditiveSemigroup, MultiplicativeSemigroup)
    deriving newtype (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)

-- Amount of a token
newtype Amount a = Amount {unAmount :: Integer}
    deriving stock (Haskell.Show, Generic)
    deriving newtype (ToJSON, FromJSON, Eq, Ord, PrintfArg)
    deriving newtype (Haskell.Eq, Haskell.Ord, Haskell.Num)
    deriving newtype (AdditiveGroup, AdditiveMonoid, AdditiveSemigroup, MultiplicativeSemigroup)
    deriving newtype (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)

{-# INLINEABLE amountOf #-}
amountOf :: Value -> Coin a -> Amount a
amountOf v = Amount . assetClassValueOf v . unCoin

{-# INLINEABLE isUnit #-}
isUnit :: Value -> Coin a -> Bool
isUnit v c = amountOf v c == 1

{-# INLINEABLE coinAmountValue #-}
coinAmountValue :: Coin a -> Amount a -> Value
coinAmountValue (Coin ac) (Amount v) = assetClassValue ac v

{-# INLINEABLE isAda #-}
isAda :: Coin a -> Bool
isAda (Coin cls) = cls == adaAssetClass
