{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Cardano.Node.Protocol.Types
  ( BlockType(..)
  , Protocol(..)
  , SomeConsensusProtocol(..)
  ) where

import           Cardano.Prelude

import           Control.Monad.Fail (fail)
import           Data.Aeson
import           NoThunks.Class (NoThunks)

import qualified Cardano.Api.Protocol.Types as Cardano
-- prototypes
import           Ouroboros.Consensus.Example.Block (ExampleBlock)
--

import           Cardano.Tracing.Constraints (TraceConstraints)
import           Cardano.Tracing.Metrics (HasKESMetricsData, HasKESInfo)

data BlockType blk where
  ByronBlockType :: BlockType ByronBlockHFC
  ShelleyBlockType :: BlockType (ShelleyBlockHFC StandardShelley)
  CardanoBlockType :: BlockType (Cardano.CardanoBlock StandardCrypto)
  -- prototypes
  ExampleBlockType :: BlockType (ExampleBlock StandardCrypto)

deriving instance Eq (BlockType blk)
deriving instance Show (BlockType blk)

data Protocol = ByronProtocol
              | ShelleyProtocol
              | CardanoProtocol
              -- prototypes
              | ExampleProtocol
  deriving (Eq, Show, Generic)

deriving instance NFData Protocol
deriving instance NoThunks Protocol

instance FromJSON Protocol where
  parseJSON =
    withText "Protocol" $ \str -> case str of

      -- The new names
      "Byron" -> pure ByronProtocol
      "Shelley" -> pure ShelleyProtocol
      "Cardano" -> pure CardanoProtocol
      -- prototypes
      "Example" -> pure ExampleProtocol

      -- The old names
      "RealPBFT" -> pure ByronProtocol
      "TPraos" -> pure ShelleyProtocol

      _ -> fail $ "Parsing of Protocol failed. "
                <> show str <> " is not a valid protocol"



data SomeConsensusProtocol where

     SomeConsensusProtocol :: forall blk. ( Cardano.Protocol IO blk
                                          , HasKESMetricsData blk
                                          , HasKESInfo blk
                                          , TraceConstraints blk
                                          )
                           => Cardano.BlockType blk
                           -> Cardano.ProtocolInfoArgs IO blk
                           -> SomeConsensusProtocol
