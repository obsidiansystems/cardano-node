{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Byron.Tx
  ( txTests
  ) where

import           Cardano.CLI.Byron.Tx
import           Cardano.Prelude
import qualified Data.Text as Text
import           Hedgehog (Property, (===))
import qualified Hedgehog as H
import           Hedgehog.Internal.Property (failWith)
import           Test.OptParse

import qualified Hedgehog.Extras.Test.Base as H

{- HLINT ignore "Use camelCase" -}

golden_byronTx_legacy :: Property
golden_byronTx_legacy = propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
  signingKey <- noteInputFile "test/data/golden/byron/keys/legacy.skey"
  goldenTx <- noteInputFile "test/data/golden/byron/tx/legacy.tx"
  createdTx <- noteTempFile tempDir "tx"
  void $ execCardanoCLI
    [ "byron","transaction","issue-utxo-expenditure"
    , "--mainnet"
    , "--byron-legacy-formats"
    , "--wallet-key", signingKey
    , "--tx", createdTx
    , "--txin", "(\"796a90e0a89b292d53a6129b9f0d757429063b529d27e4f56565192a8c8da5e3\",10)"
    , "--txout", "(\"2657WMsDfac6eFirdvKVPVMxNVYuACd1RGM2arH3g1y1yaQCr1yYpb2jr2b2aSiDZ\",999)"
    ]

  eCreatedATxAuxBS <- liftIO . runExceptT $ readByronTx $ TxFile createdTx
  createdATxAuxBS <- case eCreatedATxAuxBS of
                      Left err -> failWith Nothing . Text.unpack $ renderByronTxError err
                      Right createdATxAuxBS -> return createdATxAuxBS

  eGoldenATxAuxBS <- liftIO . runExceptT $ readByronTx $ TxFile goldenTx
  goldenATxAuxBS <- case eGoldenATxAuxBS of
                      Left err -> failWith Nothing . Text.unpack $ renderByronTxError err
                      Right goldenATxAuxBS -> return goldenATxAuxBS

  normalByronTxToGenTx createdATxAuxBS === normalByronTxToGenTx goldenATxAuxBS


golden_byronTx :: Property
golden_byronTx = propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
  signingKey <- noteInputFile "test/data/golden/byron/keys/byron.skey"
  goldenTx <- noteInputFile "test/data/golden/byron/tx/normal.tx"
  createdTx <- noteTempFile tempDir "tx"
  void $ execCardanoCLI
    [ "byron","transaction","issue-utxo-expenditure"
    , "--mainnet"
    , "--byron-formats"
    , "--wallet-key", signingKey
    , "--tx", createdTx
    , "--txin", "(\"796a90e0a89b292d53a6129b9f0d757429063b529d27e4f56565192a8c8da5e3\",10)"
    , "--txout", "(\"2657WMsDfac6eFirdvKVPVMxNVYuACd1RGM2arH3g1y1yaQCr1yYpb2jr2b2aSiDZ\",999)"
    ]

  eCreatedATxAuxBS <- liftIO . runExceptT $ readByronTx $ TxFile createdTx
  createdATxAuxBS <- case eCreatedATxAuxBS of
                      Left err -> failWith Nothing . Text.unpack $ renderByronTxError err
                      Right createdATxAuxBS -> return createdATxAuxBS

  eGoldenATxAuxBS <- liftIO . runExceptT $ readByronTx $ TxFile goldenTx
  goldenATxAuxBS <- case eGoldenATxAuxBS of
                      Left err -> failWith Nothing . Text.unpack $ renderByronTxError err
                      Right goldenATxAuxBS -> return goldenATxAuxBS

  normalByronTxToGenTx createdATxAuxBS === normalByronTxToGenTx goldenATxAuxBS

txTests :: IO Bool
txTests =
  H.checkSequential
    $ H.Group "Byron Tx Goldens"
        [ ("golden_byronTx", golden_byronTx)
        , ("golden_byronTx_legacy", golden_byronTx_legacy)
        ]
