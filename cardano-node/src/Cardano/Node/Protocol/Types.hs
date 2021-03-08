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

import qualified Ouroboros.Consensus.Cardano as Cardano
import           Ouroboros.Consensus.Cardano.Block (StandardCrypto, StandardShelley)
import           Ouroboros.Consensus.Cardano.ByronHFC (ByronBlockHFC)
import           Ouroboros.Consensus.Shelley.ShelleyHFC (ShelleyBlockHFC)

import           Cardano.Tracing.Constraints (TraceConstraints)
import           Cardano.Tracing.Metrics (HasKESMetricsData, HasKESInfo)

data BlockType blk where
  ByronBlockType :: BlockType ByronBlockHFC
  ShelleyBlockType :: BlockType (ShelleyBlockHFC StandardShelley)
  CardanoBlockType :: BlockType (Cardano.CardanoBlock StandardCrypto)

deriving instance Eq (BlockType blk)
deriving instance Show (BlockType blk)

data Protocol = ByronProtocol
              | ShelleyProtocol
              | CardanoProtocol
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

      -- The old names
      "RealPBFT" -> pure ByronProtocol
      "TPraos" -> pure ShelleyProtocol

      _ -> fail $ "Parsing of Protocol failed. "
                <> show str <> " is not a valid protocol"

data SomeConsensusProtocol where

     SomeConsensusProtocol :: forall blk p. ( Cardano.Protocol IO blk p
                                            , HasKESMetricsData blk
                                            , HasKESInfo blk
                                            , TraceConstraints blk
                                            )
                           => BlockType blk
                           -> Cardano.RunProtocol IO blk p
                           -> SomeConsensusProtocol
