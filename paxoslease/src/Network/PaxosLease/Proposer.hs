{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Network.PaxosLease.Proposer (
      ProposerState
    , Event(..)
    , handleEvent
    , ProposerConfig(..)
    , ProposerF(..)

    , PrepareRequest(..)
    , PrepareResponse(..)
    , ProposeRequest(..)
    , ProposeResponse(..)

    , BallotNumber(..)
    , Proposal(..)

    , initState
    , propose
    ) where

import Data.Typeable (Typeable)

import Control.Monad.Free (MonadFree)
import Control.Monad.Reader.Class (MonadReader)

import Control.Monad.Indexed.State (IxStateT, runIxStateT)

import Control.Monad.Indexed.State.Orphans ()

import Network.PaxosLease.Proposer.FSM hiding (initState, propose)
import qualified Network.PaxosLease.Proposer.FSM as FSM
import Network.PaxosLease.Types

data Event = ReceivedPrepareResponse PrepareResponse
           | ReceivedProposeResponse ProposeResponse
           | Timeout
    deriving (Show, Eq, Typeable)

data ProposerState = Idle IdleState
                   | Preparing PreparingState
                   | Proposing ProposingState
                   | LeaseOwner LeaseOwnerState
    deriving (Show, Eq, Typeable)

class WrappableState a where
    wrapState :: a -> ProposerState

instance WrappableState IdleState where
    wrapState = Idle

instance WrappableState PreparingState where
    wrapState = Preparing

instance WrappableState ProposingState where
    wrapState = Proposing

instance WrappableState LeaseOwnerState where
    wrapState = LeaseOwner


execIxStateT :: Functor m => IxStateT m i j () -> i -> m j
execIxStateT a s = snd `fmap` runIxStateT a s

handleEvent :: ( Monad m
               , MonadFree ProposerF m
               , MonadReader ProposerConfig m
               )
            => ProposerState
            -> Event
            -> m ProposerState
handleEvent state event = case state of
    Idle _ -> return state
    Preparing preparingState -> case event of
        ReceivedPrepareResponse response ->
            either wrapState wrapState <$> execIxStateT (onPrepareResponse response) preparingState
        _ -> return state
    Proposing proposingState -> case event of
        ReceivedProposeResponse response ->
            either wrapState wrapState <$> execIxStateT (onProposeResponse response) proposingState
        Timeout ->
            wrapState <$> execIxStateT onTimeout proposingState
        _ -> return state
    LeaseOwner leaseOwnerState -> case event of
        Timeout ->
            wrapState <$> execIxStateT onTimeout leaseOwnerState
        _ -> return state

initState :: ProposerState
initState = wrapState FSM.initState

propose :: (MonadFree ProposerF m)
        => ProposerState
        -> m ProposerState
propose = \case
    Idle idleState -> wrapState <$> execIxStateT FSM.propose idleState
    state -> return state
