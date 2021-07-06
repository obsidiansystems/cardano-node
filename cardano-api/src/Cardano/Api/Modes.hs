{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Consensus modes. The node supports several different modes with different
-- combinations of consensus protocols and ledger eras.
--
module Cardano.Api.Modes (

    -- * Consensus modes
    ByronMode,
    ShelleyMode,
    CardanoMode,
    PrototypeMode,
    ConsensusMode(..),
    AnyConsensusMode(..),
    ConsensusModeIsMultiEra(..),

    -- * The eras supported by each mode
    EraInMode(..),
    eraInModeToEra,
    anyEraInModeToAnyEra,
    AnyEraInMode(..),
    toEraInMode,

    -- * Connection paramaters for each mode
    ConsensusModeParams(..),
    AnyConsensusModeParams(..),
    Byron.EpochSlots(..),

    -- * Conversions to and from types in the consensus library
    ConsensusBlockForMode,
    ConsensusBlockForEra,
    toConsensusEraIndex,
    fromConsensusEraIndex,
  ) where

import           Prelude

import           Data.SOP.Strict (K (K), NS (S, Z))

import qualified Ouroboros.Consensus.Byron.Ledger as Consensus
import qualified Ouroboros.Consensus.Cardano.Block as Consensus
import qualified Ouroboros.Consensus.Cardano.ByronHFC as Consensus (ByronBlockHFC)
import qualified Ouroboros.Consensus.Shelley.ShelleyHFC as Consensus (ShelleyBlockHFC)
import           Ouroboros.Consensus.HardFork.Combinator as Consensus (EraIndex (..), eraIndexSucc,
                   eraIndexZero)
import           Ouroboros.Consensus.Shelley.Eras (StandardAllegra, StandardMary, StandardShelley)
import qualified Ouroboros.Consensus.Shelley.Ledger as Consensus
import           Ouroboros.Consensus.Shelley.Protocol (StandardCrypto)
-- prototypes
import qualified Ouroboros.Consensus.Voltaire.Prototype as Consensus
import qualified Ouroboros.Consensus.Voltaire.Prototype.Block as Consensus

import qualified Cardano.Chain.Slotting as Byron (EpochSlots (..))

import           Cardano.Api.Eras
import           Cardano.Api.Prototype.Tmp (StandardVoltaireOne, StandardVoltaireTwo)


-- ----------------------------------------------------------------------------
-- Consensus modes
--

-- | The Byron-only consensus mode consists of only the Byron era.
--
-- This was used on the mainnet before the deployment of the multi-era
-- 'CardanoMode'. It is now of little practical use, though it illustrates
-- how a single-era consensus mode works. It may be sensible to remove this
-- at some stage.
--
data ByronMode

-- | The Shelley-only consensus mode consists of only the Shelley era.
--
-- This was used for the early Shelley testnets prior to the use of the
-- multi-era 'CardanoMode'. It is useful for setting up Shelley test networks
-- (e.g. for benchmarking) without having to go through the complication of the
-- hard fork from Byron to Shelley eras. It also shows how a single-era
-- consensus mode works. It may be replaced by other single-era modes in future.
--
data ShelleyMode

-- | The Cardano consensus mode consists of all the eras currently in use on
-- the Cardano mainnet. This is currently: the 'ByronEra'; 'ShelleyEra',
-- 'AllegraEra' and 'MaryEra', in that order.
--
-- This mode will be extended with new eras as the Cardano mainnet develops.
--
data CardanoMode

-- prototypes
data PrototypeMode

data AnyConsensusModeParams where
  AnyConsensusModeParams :: ConsensusModeParams mode -> AnyConsensusModeParams

deriving instance Show AnyConsensusModeParams

-- | This GADT provides a value-level representation of all the consensus modes.
-- This enables pattern matching on the era to allow them to be treated in a
-- non-uniform way.
--
data ConsensusMode mode where
     ByronMode   :: ConsensusMode ByronMode
     ShelleyMode :: ConsensusMode ShelleyMode
     CardanoMode :: ConsensusMode CardanoMode
     PrototypeMode :: ConsensusMode PrototypeMode

deriving instance Show (ConsensusMode mode)

data AnyConsensusMode where
  AnyConsensusMode :: ConsensusMode mode -> AnyConsensusMode

deriving instance Show AnyConsensusMode


-- | The subset of consensus modes that consist of multiple eras. Some features
-- are not supported in single-era modes (for exact compatibility with not
-- using the hard fork combinatior at all).
--
data ConsensusModeIsMultiEra mode where
     CardanoModeIsMultiEra :: ConsensusModeIsMultiEra CardanoMode
     PrototypeModeIsMultiEra :: ConsensusModeIsMultiEra PrototypeMode

deriving instance Show (ConsensusModeIsMultiEra mode)

toEraInMode :: CardanoEra era -> ConsensusMode mode -> Maybe (EraInMode era mode)
toEraInMode ByronEra   ByronMode   = Just ByronEraInByronMode
toEraInMode ShelleyEra ShelleyMode = Just ShelleyEraInShelleyMode
toEraInMode ByronEra   CardanoMode = Just ByronEraInCardanoMode
toEraInMode ShelleyEra CardanoMode = Just ShelleyEraInCardanoMode
toEraInMode AllegraEra CardanoMode = Just AllegraEraInCardanoMode
toEraInMode MaryEra    CardanoMode = Just MaryEraInCardanoMode
toEraInMode ShelleyEra PrototypeMode = Just ShelleyEraInPrototypeMode
toEraInMode VoltairePrototypeOneEra PrototypeMode = Just VoltairePrototypeOneEraInPrototypeMode
toEraInMode _ _                    = Nothing


-- | A representation of which 'CardanoEra's are included in each
-- 'ConsensusMode'.
--
data EraInMode era mode where
     ByronEraInByronMode     :: EraInMode ByronEra   ByronMode

     ShelleyEraInShelleyMode :: EraInMode ShelleyEra ShelleyMode

     ByronEraInCardanoMode   :: EraInMode ByronEra   CardanoMode
     ShelleyEraInCardanoMode :: EraInMode ShelleyEra CardanoMode
     AllegraEraInCardanoMode :: EraInMode AllegraEra CardanoMode
     MaryEraInCardanoMode    :: EraInMode MaryEra    CardanoMode
      -- prototypes in prototype consensus modes
     ShelleyEraInPrototypeMode :: EraInMode ShelleyEra PrototypeMode
     VoltairePrototypeOneEraInPrototypeMode :: EraInMode VoltairePrototypeOneEra PrototypeMode
     VoltairePrototypeTwoEraInPrototypeMode :: EraInMode VoltairePrototypeTwoEra PrototypeMode

deriving instance Show (EraInMode era mode)


eraInModeToEra :: EraInMode era mode -> CardanoEra era
eraInModeToEra ByronEraInByronMode     = ByronEra
eraInModeToEra ShelleyEraInShelleyMode = ShelleyEra
eraInModeToEra ByronEraInCardanoMode   = ByronEra
eraInModeToEra ShelleyEraInCardanoMode = ShelleyEra
eraInModeToEra AllegraEraInCardanoMode = AllegraEra
eraInModeToEra MaryEraInCardanoMode    = MaryEra
eraInModeToEra ShelleyEraInPrototypeMode = ShelleyEra
eraInModeToEra VoltairePrototypeOneEraInPrototypeMode = VoltairePrototypeOneEra
eraInModeToEra VoltairePrototypeTwoEraInPrototypeMode = VoltairePrototypeTwoEra

data AnyEraInMode mode where
     AnyEraInMode :: EraInMode era mode -> AnyEraInMode mode

deriving instance Show (AnyEraInMode mode)


anyEraInModeToAnyEra :: AnyEraInMode mode -> AnyCardanoEra
anyEraInModeToAnyEra (AnyEraInMode erainmode) =
  case erainmode of
    ByronEraInByronMode     -> AnyCardanoEra ByronEra
    ShelleyEraInShelleyMode -> AnyCardanoEra ShelleyEra
    ByronEraInCardanoMode   -> AnyCardanoEra ByronEra
    ShelleyEraInCardanoMode -> AnyCardanoEra ShelleyEra
    AllegraEraInCardanoMode -> AnyCardanoEra AllegraEra
    MaryEraInCardanoMode    -> AnyCardanoEra MaryEra
    ShelleyEraInPrototypeMode -> AnyCardanoEra ShelleyEra
    VoltairePrototypeOneEraInPrototypeMode -> AnyCardanoEra VoltairePrototypeOneEra
    VoltairePrototypeTwoEraInPrototypeMode -> AnyCardanoEra VoltairePrototypeTwoEra


-- | The consensus-mode-specific parameters needed to connect to a local node
-- that is using each consensus mode.
--
-- It is in fact only the Byron era that requires extra parameters, but this is
-- of course inherited by the 'CardanoMode' that uses the Byron era. The reason
-- this parameter is needed stems from unfortunate design decisions from the
-- legacy Byron era. The slots per epoch are needed to be able to /decode/
-- epoch boundary blocks from the Byron era.
--
-- It is possible in future that we may be able to eliminate this parameter by
-- discovering it from the node during the initial handshake.
--
data ConsensusModeParams mode where

     ByronModeParams
       :: Byron.EpochSlots
       -> ConsensusModeParams ByronMode

     ShelleyModeParams
       :: ConsensusModeParams ShelleyMode

     CardanoModeParams
       :: Byron.EpochSlots
       -> ConsensusModeParams CardanoMode

     -- prototypes
     ExampleModeParams
       :: ConsensusModeParams PrototypeMode

deriving instance Show (ConsensusModeParams mode)

-- ----------------------------------------------------------------------------
-- Consensus conversion functions
--

-- | A closed type family that maps between the consensus mode (from this API)
-- and the block type used by the consensus libraries.
--
type family ConsensusBlockForMode mode where
  ConsensusBlockForMode ByronMode   = Consensus.ByronBlockHFC
  ConsensusBlockForMode ShelleyMode = Consensus.ShelleyBlockHFC StandardShelley
  ConsensusBlockForMode CardanoMode = Consensus.CardanoBlock StandardCrypto
  ConsensusBlockForMode PrototypeMode = Consensus.VoltairePrototypeBlock StandardCrypto

type family ConsensusBlockForEra era where
  ConsensusBlockForEra ByronEra   = Consensus.ByronBlock
  ConsensusBlockForEra ShelleyEra = Consensus.ShelleyBlock StandardShelley
  ConsensusBlockForEra AllegraEra = Consensus.ShelleyBlock StandardAllegra
  ConsensusBlockForEra MaryEra    = Consensus.ShelleyBlock StandardMary
  ConsensusBlockForEra VoltairePrototypeOneEra = Consensus.ShelleyBlock StandardVoltaireOne
  ConsensusBlockForEra VoltairePrototypeTwoEra = Consensus.ShelleyBlock StandardVoltaireTwo



eraIndex0 :: Consensus.EraIndex (x0 : xs)
eraIndex0 = Consensus.eraIndexZero

eraIndex1 :: Consensus.EraIndex (x1 : x0 : xs)
eraIndex1 = eraIndexSucc eraIndex0

eraIndex2 :: Consensus.EraIndex (x2 : x1 : x0 : xs)
eraIndex2 = eraIndexSucc eraIndex1

eraIndex3 :: Consensus.EraIndex (x3 : x2 : x1 : x0 : xs)
eraIndex3 = eraIndexSucc eraIndex2


toConsensusEraIndex :: ConsensusBlockForMode mode ~ Consensus.HardForkBlock xs
                    => EraInMode era mode
                    -> Consensus.EraIndex xs
toConsensusEraIndex ByronEraInByronMode     = eraIndex0
toConsensusEraIndex ShelleyEraInShelleyMode = eraIndex0

toConsensusEraIndex ByronEraInCardanoMode   = eraIndex0
toConsensusEraIndex ShelleyEraInCardanoMode = eraIndex1
toConsensusEraIndex AllegraEraInCardanoMode = eraIndex2
toConsensusEraIndex MaryEraInCardanoMode    = eraIndex3

toConsensusEraIndex ShelleyEraInPrototypeMode = eraIndex0
toConsensusEraIndex VoltairePrototypeOneEraInPrototypeMode = eraIndex1
toConsensusEraIndex VoltairePrototypeTwoEraInPrototypeMode = eraIndex2

fromConsensusEraIndex :: ConsensusBlockForMode mode ~ Consensus.HardForkBlock xs
                      => ConsensusMode mode
                      -> Consensus.EraIndex xs
                      -> AnyEraInMode mode
fromConsensusEraIndex ByronMode = fromByronEraIndex
  where
    fromByronEraIndex :: Consensus.EraIndex
                           '[Consensus.ByronBlock]
                      -> AnyEraInMode ByronMode
    fromByronEraIndex (Consensus.EraIndex (Z (K ()))) =
      AnyEraInMode ByronEraInByronMode

fromConsensusEraIndex ShelleyMode = fromShelleyEraIndex
  where
    fromShelleyEraIndex :: Consensus.EraIndex
                             '[Consensus.ShelleyBlock StandardShelley]
                        -> AnyEraInMode ShelleyMode
    fromShelleyEraIndex (Consensus.EraIndex (Z (K ()))) =
      AnyEraInMode ShelleyEraInShelleyMode


fromConsensusEraIndex CardanoMode = fromShelleyEraIndex
  where
    fromShelleyEraIndex :: Consensus.EraIndex
                             (Consensus.CardanoEras StandardCrypto)
                        -> AnyEraInMode CardanoMode
    fromShelleyEraIndex (Consensus.EraIndex (Z (K ()))) =
      AnyEraInMode ByronEraInCardanoMode

    fromShelleyEraIndex (Consensus.EraIndex (S (Z (K ())))) =
      AnyEraInMode ShelleyEraInCardanoMode

    fromShelleyEraIndex (Consensus.EraIndex (S (S (Z (K ()))))) =
      AnyEraInMode AllegraEraInCardanoMode

    fromShelleyEraIndex (Consensus.EraIndex (S (S (S (Z (K ())))))) =
      AnyEraInMode MaryEraInCardanoMode

fromConsensusEraIndex PrototypeMode = fromPrototypeEraIndex
  where
    fromPrototypeEraIndex :: Consensus.EraIndex
                             (Consensus.VoltairePrototypeEras StandardCrypto)
                        -> AnyEraInMode PrototypeMode
    fromPrototypeEraIndex (Consensus.EraIndex (Z (K ()))) =
      AnyEraInMode ShelleyEraInPrototypeMode

    fromPrototypeEraIndex (Consensus.EraIndex (S (Z (K ())))) =
      AnyEraInMode VoltairePrototypeOneEraInPrototypeMode

    fromPrototypeEraIndex (Consensus.EraIndex (S (S (Z (K ()))))) =
      AnyEraInMode VoltairePrototypeTwoEraInPrototypeMode
