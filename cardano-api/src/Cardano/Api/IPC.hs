{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- The Shelley ledger uses promoted data kinds which we have to use, but we do
-- not export any from this API. We also use them unticked as nature intended.
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

-- | Node IPC protocols
--
module Cardano.Api.IPC (

    -- * Node interaction
    -- | Operations that involve talking to a local Cardano node.
    connectToLocalNode,
    LocalNodeConnectInfo(..),
    localConsensusMode,
    LocalNodeClientProtocols(..),
    LocalNodeClientProtocolsInMode,
--  connectToRemoteNode,
    EraMismatch(..),

    -- *** Chain sync protocol
    ChainSyncClient(..),
    BlockInMode(..),

    -- *** Local tx submission
    LocalTxSubmissionClient(..),
    TxInMode(..),
    TxValidationErrorInMode(..),
    TxValidationError,
    submitTxToNodeLocal,
    SubmitResult(..),

    -- *** Local state query
    LocalStateQueryClient(..),
    QueryInMode(..),
    QueryInEra(..),
    QueryInShelleyBasedEra(..),
    queryNodeLocalState,
  ) where

import           Prelude

import           Data.Void (Void)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import           Data.SOP.Strict (NS(Z, S))

import           Control.Concurrent.STM
import           Control.Tracer (nullTracer)

-- TODO: it'd be nice if the network imports needed were a bit more coherent
import           Ouroboros.Network.Block as Consensus (Point, Tip(..))
import           Ouroboros.Network.Mux
import           Ouroboros.Network.NodeToClient
import           Ouroboros.Network.Protocol.ChainSync.Client as ChainSync
import           Ouroboros.Network.Protocol.LocalStateQuery.Client as StateQuery
import           Ouroboros.Network.Protocol.LocalStateQuery.Type (AcquireFailure)
import           Ouroboros.Network.Protocol.LocalTxSubmission.Client as TxSubmission
import           Ouroboros.Network.Util.ShowProxy (ShowProxy)

-- TODO: it'd be nice if the consensus imports needed were a bit more coherent
import           Ouroboros.Consensus.Block as Consensus
                   (BlockProtocol, CodecConfig)
import           Ouroboros.Consensus.Cardano (ProtocolClient(..), protocolClientInfo)
import           Ouroboros.Consensus.Cardano.Node ()
import           Ouroboros.Consensus.Ledger.Query as Consensus (ShowQuery)
import           Ouroboros.Consensus.Ledger.SupportsMempool as Consensus (ApplyTxErr, GenTx)
import           Ouroboros.Consensus.Network.NodeToClient
                   (Codecs' (..), clientCodecs)
import           Ouroboros.Consensus.Node.NetworkProtocolVersion (BlockNodeToClientVersion,
                     SupportedNetworkProtocolVersion, supportedNodeToClientVersions)
import           Ouroboros.Consensus.Node.ProtocolInfo (ProtocolClientInfo (..))
import           Ouroboros.Consensus.Node.Run (SerialiseNodeToClientConstraints)
import           Ouroboros.Consensus.HardFork.Combinator as Consensus
                   (GenTx(HardForkGenTx), OneEraGenTx(OneEraGenTx))
import           Ouroboros.Consensus.HardFork.Combinator.Degenerate
                   (HardForkApplyTxErr(DegenApplyTxErr),
                    HardForkBlock(DegenBlock))
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras as Consensus
                   (EraMismatch(..))

import qualified Ouroboros.Consensus.Byron.Ledger       as Consensus
import qualified Ouroboros.Consensus.Shelley.Ledger     as Consensus
import qualified Ouroboros.Consensus.Cardano.Block      as Consensus

import           Cardano.Api.Block
import           Cardano.Api.Eras
import           Cardano.Api.HasTypeProxy
import           Cardano.Api.Modes
import           Cardano.Api.Query

import           Cardano.Api.Typed (NetworkId, toNetworkMagic, Tx(..))


-- ----------------------------------------------------------------------------
-- Types used in the node-to-client IPC protocols
--

-- | A 'Block' in one of the eras supported by a given protocol mode.
--
-- For multi-era modes such as the 'CardanoMode' this type is a sum of the
-- different block types for all the eras. It is used in the ChainSync protocol.
--
data BlockInMode mode where
     BlockInMode :: Block era -> EraInMode era mode -> BlockInMode mode

deriving instance Show (BlockInMode mode)


-- | A 'Tx' in one of the eras supported by a given protocol mode.
--
-- For multi-era modes such as the 'CardanoMode' this type is a sum of the
-- different transaction types for all the eras. It is used in the
-- LocalTxSubmission protocol.
--
data TxInMode mode where

     -- | Everything we consider a normal transaction.
     --
     TxInMode :: Tx era -> EraInMode era mode -> TxInMode mode

     -- | Byron has various things we can post to the chain which are not
     -- actually transactions. This covers: update proposals, votes and
     -- delegation certs.
     --
     TxInByronSpecial :: Consensus.GenTx Consensus.ByronBlock
                      -> EraInMode ByronEra mode -> TxInMode mode

deriving instance Show (TxInMode mode)


-- | The transaction validations errors that can occur from trying to submit a
-- transaction to a local node. The errors are specific to an era.
--
data TxValidationError era where

     ByronTxValidationError
       :: ApplyTxErr Consensus.ByronBlock
       -> TxValidationError ByronEra

     ShelleyTxValidationError
       :: ShelleyBasedEra era
       -> ApplyTxErr (Consensus.ShelleyBlock (ShelleyLedgerEra era))
       -> TxValidationError era

-- The GADT in the ShelleyTxValidationError case requires a custom instance
instance Show (TxValidationError era) where
    showsPrec p (ByronTxValidationError err) =
      showParen (p >= 11)
        ( showString "ByronTxValidationError "
        . showsPrec 11 err
        )

    showsPrec p (ShelleyTxValidationError ShelleyBasedEraShelley err) =
      showParen (p >= 11)
        ( showString "ShelleyTxValidationError ShelleyBasedEraShelley "
        . showsPrec 11 err
        )

    showsPrec p (ShelleyTxValidationError ShelleyBasedEraAllegra err) =
      showParen (p >= 11)
        ( showString "ShelleyTxValidationError ShelleyBasedEraAllegra "
        . showsPrec 11 err
        )

    showsPrec p (ShelleyTxValidationError ShelleyBasedEraMary err) =
      showParen (p >= 11)
        ( showString "ShelleyTxValidationError ShelleyBasedEraMary "
        . showsPrec 11 err
        )


-- | A 'TxValidationError' in one of the eras supported by a given protocol
-- mode.
--
-- This is used in the LocalStateQuery protocol.
--
data TxValidationErrorInMode mode where
     TxValidationErrorInMode :: TxValidationError era
                             -> EraInMode era mode
                             -> TxValidationErrorInMode mode

     TxValidationEraMismatch :: EraMismatch
                             -> TxValidationErrorInMode mode

deriving instance Show (TxValidationErrorInMode mode)


-- ----------------------------------------------------------------------------
-- The types for the client side of the node-to-client IPC protocols
--

data LocalNodeClientProtocols block point tip tx txerr query m =
     LocalNodeClientProtocols {
       localChainSyncClient    :: Maybe (ChainSyncClient         block point tip   m ())
     , localTxSubmissionClient :: Maybe (LocalTxSubmissionClient tx txerr          m ())
     , localStateQueryClient   :: Maybe (LocalStateQueryClient   block point query m ())
     }

-- public, exported
type LocalNodeClientProtocolsInMode mode =
       LocalNodeClientProtocols
         (BlockInMode mode)
         ChainPoint
         ChainTip
         (TxInMode mode)
         (TxValidationErrorInMode mode)
         (QueryInMode mode)
         IO

-- internal, consensus
type LocalNodeClientProtocolsForBlock block =
       LocalNodeClientProtocols
         block
         (Consensus.Point block)
         (Consensus.Tip block)
         (Consensus.GenTx block)
         (Consensus.ApplyTxErr block)
         (Consensus.Query block)
         IO

data LocalNodeConnectInfo mode =
     LocalNodeConnectInfo {
       localConsensusModeParams :: ConsensusModeParams mode,
       localNodeNetworkId       :: NetworkId,
       localNodeSocketPath      :: FilePath
     }

localConsensusMode :: LocalNodeConnectInfo mode -> ConsensusMode mode
localConsensusMode LocalNodeConnectInfo {localConsensusModeParams} =
    consensusModeOnly localConsensusModeParams

consensusModeOnly :: ConsensusModeParams mode
                  -> ConsensusMode       mode
consensusModeOnly ByronModeParams{}   = ByronMode
consensusModeOnly ShelleyModeParams{} = ShelleyMode
consensusModeOnly CardanoModeParams{} = CardanoMode


-- ----------------------------------------------------------------------------
-- Actually connect to the node
--

-- | Establish a connection to a local node and execute the given set of
-- protocol handlers.
--
connectToLocalNode :: LocalNodeConnectInfo mode
                   -> LocalNodeClientProtocolsInMode mode
                   -> IO ()
connectToLocalNode LocalNodeConnectInfo {
                     localNodeSocketPath,
                     localNodeNetworkId,
                     localConsensusModeParams
                   } clients =
    withIOManager $ \iomgr ->
      connectTo
        (localSnocket iomgr localNodeSocketPath)
        NetworkConnectTracers {
          nctMuxTracer       = nullTracer,
          nctHandshakeTracer = nullTracer
        }
        versionedProtocls
        localNodeSocketPath
  where
    versionedProtocls =
      -- First convert from the mode-parametrised view of things to the
      -- block-parametrised view and then do the final setup for the versioned
      -- bundles of mini-protocols.
      case mkLocalNodeClientParams localConsensusModeParams clients of
        LocalNodeClientParams ptcl clients' ->
          mkVersionedProtocols localNodeNetworkId ptcl clients'


mkVersionedProtocols :: forall block.
                        (SerialiseNodeToClientConstraints block,
                         SupportedNetworkProtocolVersion block,
                         ShowProxy block, ShowProxy (ApplyTxErr block),
                         ShowProxy (GenTx block), ShowProxy (Consensus.Query block),
                         ShowQuery (Consensus.Query block))
                     => NetworkId
                     -> ProtocolClient block (BlockProtocol block)
                     -> LocalNodeClientProtocolsForBlock block
                     -> Versions
                          NodeToClientVersion
                          NodeToClientVersionData
                          (OuroborosApplication
                             InitiatorMode
                             LocalAddress
                             LBS.ByteString IO () Void)
mkVersionedProtocols networkid ptcl
                     LocalNodeClientProtocols {
                       localChainSyncClient,
                       localTxSubmissionClient,
                       localStateQueryClient
                     } =
     --TODO: really we should construct specific combinations of
     -- protocols for the versions we know about, with different protocol
     -- versions taking different sets of typed client protocols.
    foldMapVersions
      (\(ptclVersion, ptclBlockVersion) ->
          versionedNodeToClientProtocols
            ptclVersion
            NodeToClientVersionData {
              networkMagic = toNetworkMagic networkid
            }
            (\_connid _ctl -> protocols ptclBlockVersion))
      (Map.toList (supportedNodeToClientVersions proxy))
  where
    proxy :: Proxy block
    proxy = Proxy

    protocols :: BlockNodeToClientVersion block
              -> NodeToClientProtocols InitiatorMode LBS.ByteString IO () Void
    protocols ptclBlockVersion =
        NodeToClientProtocols {
          localChainSyncProtocol =
            InitiatorProtocolOnly $
              MuxPeer
                nullTracer
                cChainSyncCodec
                (maybe chainSyncPeerNull
                       chainSyncClientPeer localChainSyncClient)

        , localTxSubmissionProtocol =
            InitiatorProtocolOnly $
              MuxPeer
                nullTracer
                cTxSubmissionCodec
                (maybe localTxSubmissionPeerNull
                       localTxSubmissionClientPeer localTxSubmissionClient)

        , localStateQueryProtocol =
            InitiatorProtocolOnly $
              MuxPeer
                nullTracer
                cStateQueryCodec
                (maybe localStateQueryPeerNull
                       localStateQueryClientPeer localStateQueryClient)
        }
      where
        Codecs {
          cChainSyncCodec,
          cTxSubmissionCodec,
          cStateQueryCodec
        } = clientCodecs codecConfig ptclBlockVersion

    codecConfig :: CodecConfig block
    codecConfig = pClientInfoCodecConfig (protocolClientInfo ptcl)


-- | This type defines the boundary between the mode-parametrised style used in
-- this API and the block-parametrised style used by the underlying network
-- and consensus libraries.
--
-- This interface itself is in the block-parametrised style, with the block
-- type itself being an hidden\/existential type.
--
-- It bundles together all the necessary class instances, the consensus
-- protocol client identifier, and the set of client side mini-protocol
-- handlers for the node-to-client protocol.
--
data LocalNodeClientParams where
     LocalNodeClientParams
       :: (SerialiseNodeToClientConstraints block,
           SupportedNetworkProtocolVersion block,
           ShowProxy block, ShowProxy (Consensus.ApplyTxErr block),
           ShowProxy (Consensus.GenTx block), ShowProxy (Consensus.Query block),
           ShowQuery (Consensus.Query block))
       => ProtocolClient block (BlockProtocol block)
       -> LocalNodeClientProtocolsForBlock block
       -> LocalNodeClientParams


-- | Convert from the mode-parametrised style to the block-parametrised style.
--
mkLocalNodeClientParams :: forall mode block.
                           ConsensusBlockForMode mode ~ block
                        => ConsensusModeParams mode
                        -> LocalNodeClientProtocolsInMode mode
                        -> LocalNodeClientParams
mkLocalNodeClientParams modeparams clients =
    -- For each of the possible consensus modes we pick the concrete block type
    -- (by picking the appropriate 'ProtocolClient' value).
    --
    -- Though it is not immediately visible, this point where we use
    -- 'LocalNodeClientParams' is also where we pick up the necessary class
    -- instances. This works because in each case we have a monomorphic block
    -- type and the instances are all in scope. This is why the use of
    -- LocalNodeClientParams is repeated within each branch of the case:
    -- because it is only within each branch that the GADT match makes the
    -- block type monomorphic.
    --
    case modeparams of
      ByronModeParams epochSlots ->
        LocalNodeClientParams
          (ProtocolClientByron epochSlots)
          (convLocalNodeClientProtocols ByronMode clients)

      ShelleyModeParams ->
        LocalNodeClientParams
          ProtocolClientShelley
          (convLocalNodeClientProtocols ShelleyMode clients)

      CardanoModeParams epochSlots ->
        LocalNodeClientParams
          (ProtocolClientCardano epochSlots)
          (convLocalNodeClientProtocols CardanoMode clients)

convLocalNodeClientProtocols :: forall mode block.
                                ConsensusBlockForMode mode ~ block
                             => ConsensusMode mode
                             -> LocalNodeClientProtocolsInMode mode
                             -> LocalNodeClientProtocolsForBlock block
convLocalNodeClientProtocols
    mode
    LocalNodeClientProtocols {
      localChainSyncClient,
      localTxSubmissionClient,
      localStateQueryClient
    } =
    LocalNodeClientProtocols {
      localChainSyncClient    = convLocalChainSyncClient mode <$>
                                  localChainSyncClient,

      localTxSubmissionClient = convLocalTxSubmissionClient mode <$>
                                  localTxSubmissionClient,

      localStateQueryClient   = convLocalStateQueryClient mode <$>
                                  localStateQueryClient
    }


convLocalTxSubmissionClient
  :: forall mode block m a.
     (ConsensusBlockForMode mode ~ block, Functor m)
  => ConsensusMode mode
  -> LocalTxSubmissionClient (TxInMode mode) (TxValidationErrorInMode mode) m a
  -> LocalTxSubmissionClient (GenTx block) (ApplyTxErr block) m a
convLocalTxSubmissionClient mode =
    mapLocalTxSubmissionClient convTx (convErr mode)


convTx :: ConsensusBlockForMode mode ~ block
       => TxInMode mode -> GenTx block
convTx (TxInMode (ByronTx tx) ByronEraInByronMode) =
    HardForkGenTx (OneEraGenTx (Z tx'))
  where
    tx' = Consensus.ByronTx (Consensus.byronIdTx tx) tx

convTx (TxInMode (ByronTx tx) ByronEraInCardanoMode) =
    HardForkGenTx (OneEraGenTx (Z tx'))
  where
    tx' = Consensus.ByronTx (Consensus.byronIdTx tx) tx
    --TODO: add the above as mkByronTx to the consensus code,
    -- matching mkShelleyTx below

convTx (TxInByronSpecial gtx ByronEraInByronMode) =
    HardForkGenTx (OneEraGenTx (Z gtx))

convTx (TxInByronSpecial gtx ByronEraInCardanoMode) =
    HardForkGenTx (OneEraGenTx (Z gtx))

convTx (TxInMode (ShelleyTx _ tx) ShelleyEraInShelleyMode) =
    HardForkGenTx (OneEraGenTx (Z tx'))
  where
    tx' = Consensus.mkShelleyTx tx

convTx (TxInMode (ShelleyTx _ tx) ShelleyEraInCardanoMode) =
    HardForkGenTx (OneEraGenTx (S (Z tx')))
  where
    tx' = Consensus.mkShelleyTx tx

convTx (TxInMode (ShelleyTx _ tx) AllegraEraInCardanoMode) =
    HardForkGenTx (OneEraGenTx (S (S (Z tx'))))
  where
    tx' = Consensus.mkShelleyTx tx

convTx (TxInMode (ShelleyTx _ tx) MaryEraInCardanoMode) =
    HardForkGenTx (OneEraGenTx (S (S (S (Z tx')))))
  where
    tx' = Consensus.mkShelleyTx tx


convErr :: ConsensusBlockForMode mode ~ block
        => ConsensusMode mode
        -> ApplyTxErr block -> TxValidationErrorInMode mode
convErr ByronMode (DegenApplyTxErr err) =
    TxValidationErrorInMode
      (ByronTxValidationError err)
      ByronEraInByronMode

convErr ShelleyMode (DegenApplyTxErr err) =
    TxValidationErrorInMode
      (ShelleyTxValidationError ShelleyBasedEraShelley err)
      ShelleyEraInShelleyMode

convErr CardanoMode (Consensus.ApplyTxErrByron err) =
    TxValidationErrorInMode
      (ByronTxValidationError err)
      ByronEraInCardanoMode

convErr CardanoMode (Consensus.ApplyTxErrShelley err) =
    TxValidationErrorInMode
      (ShelleyTxValidationError ShelleyBasedEraShelley err)
      ShelleyEraInCardanoMode

convErr CardanoMode (Consensus.ApplyTxErrAllegra err) =
    TxValidationErrorInMode
      (ShelleyTxValidationError ShelleyBasedEraAllegra err)
      AllegraEraInCardanoMode

convErr CardanoMode (Consensus.ApplyTxErrMary err) =
    TxValidationErrorInMode
      (ShelleyTxValidationError ShelleyBasedEraMary err)
      MaryEraInCardanoMode

convErr CardanoMode (Consensus.ApplyTxErrWrongEra err) =
    TxValidationEraMismatch err


convLocalChainSyncClient
  :: forall mode block m a.
     (ConsensusBlockForMode mode ~ block, Functor m)
  => ConsensusMode mode
  -> ChainSyncClient (BlockInMode mode) ChainPoint ChainTip m a
  -> ChainSyncClient block (Point block) (Tip block) m a

convLocalChainSyncClient mode =
    mapChainSyncClient (toConsensusPointInMode mode)
                       (fromConsensusPointInMode mode)
                       (convBlock mode) (fromConsensusTip mode)


convBlock :: ConsensusBlockForMode mode ~ block
          => ConsensusMode mode -> block -> BlockInMode mode
convBlock ByronMode =
    \b -> case b of
      DegenBlock b' -> BlockInMode (ByronBlock b')
                                   ByronEraInByronMode
convBlock ShelleyMode =
    \b -> case b of
      DegenBlock b' -> BlockInMode (ShelleyBlock ShelleyBasedEraShelley b')
                                   ShelleyEraInShelleyMode
convBlock CardanoMode =
    \b -> case b of
      Consensus.BlockByron   b' -> BlockInMode (ByronBlock   b')
                                               ByronEraInCardanoMode
      Consensus.BlockShelley b' -> BlockInMode (ShelleyBlock ShelleyBasedEraShelley b')
                                               ShelleyEraInCardanoMode
      Consensus.BlockAllegra b' -> BlockInMode (ShelleyBlock ShelleyBasedEraAllegra b')
                                               AllegraEraInCardanoMode
      Consensus.BlockMary    b' -> BlockInMode (ShelleyBlock ShelleyBasedEraMary b')
                                               MaryEraInCardanoMode


convLocalStateQueryClient
  :: forall mode block m a.
     (ConsensusBlockForMode mode ~ block, Functor m)
  => ConsensusMode mode
  -> LocalStateQueryClient (BlockInMode mode) ChainPoint (QueryInMode mode) m a
  -> LocalStateQueryClient block (Consensus.Point block)
                           (Consensus.Query block) m a
convLocalStateQueryClient mode =
    mapLocalStateQueryClient
      (toConsensusPointInMode mode)
      toConsensusQuery
      fromConsensusQueryResult


-- ----------------------------------------------------------------------------
-- Wrappers for specific protocol use-cases
--

--TODO: change this query to be just a protocol client handler to be used with
-- connectToLocalNode. This would involve changing connectToLocalNode to be
-- able to return protocol handler results properly.

-- | Establish a connection to a node and execute a single query using the
-- local state query protocol.
--
queryNodeLocalState :: forall mode result.
                       LocalNodeConnectInfo mode
                    -> ChainPoint
                    -> QueryInMode mode result
                    -> IO (Either AcquireFailure result)
queryNodeLocalState connctInfo point query = do
    resultVar <- newEmptyTMVarIO
    connectToLocalNode
      connctInfo
      LocalNodeClientProtocols {
        localChainSyncClient    = Nothing,
        localTxSubmissionClient = Nothing,
        localStateQueryClient   = Just (localStateQuerySingle resultVar)
      }
    atomically (takeTMVar resultVar)
  where
    localStateQuerySingle
      :: TMVar (Either AcquireFailure result)
      -> LocalStateQueryClient (BlockInMode mode) ChainPoint
                               (QueryInMode mode) IO ()
    localStateQuerySingle resultVar =
      LocalStateQueryClient $ pure $
        SendMsgAcquire point $
        ClientStAcquiring {
          recvMsgAcquired =
            SendMsgQuery query $
            ClientStQuerying {
              recvMsgResult = \result -> do
                --TODO: return the result via the SendMsgDone rather than
                -- writing into an mvar
                atomically $ putTMVar resultVar (Right result)
                pure $ SendMsgRelease $
                  pure $ StateQuery.SendMsgDone ()
            }
        , recvMsgFailure = \failure -> do
            --TODO: return the result via the SendMsgDone rather than
            -- writing into an mvar
            atomically $ putTMVar resultVar (Left failure)
            pure $ StateQuery.SendMsgDone ()
        }


submitTxToNodeLocal :: forall mode.
                       LocalNodeConnectInfo mode
                    -> TxInMode mode
                    -> IO (SubmitResult (TxValidationErrorInMode mode))
submitTxToNodeLocal connctInfo tx = do
    resultVar <- newEmptyTMVarIO
    connectToLocalNode
      connctInfo
      LocalNodeClientProtocols {
        localChainSyncClient    = Nothing,
        localTxSubmissionClient = Just (localTxSubmissionClientSingle resultVar),
        localStateQueryClient   = Nothing

      }
    atomically (takeTMVar resultVar)
  where
    localTxSubmissionClientSingle
      :: TMVar (SubmitResult (TxValidationErrorInMode mode))
      -> LocalTxSubmissionClient (TxInMode mode)
                                 (TxValidationErrorInMode mode)
                                 IO ()
    localTxSubmissionClientSingle resultVar =
      LocalTxSubmissionClient $
        pure $ SendMsgSubmitTx tx $ \result -> do
        atomically $ putTMVar resultVar result
        pure (TxSubmission.SendMsgDone ())

