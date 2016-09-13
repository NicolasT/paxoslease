{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Network.PaxosLease.Proposer.FSM (
      ProposerConfig(..)
    , ProposerF(..)
    , IdleState
    , PreparingState
    , ProposingState
    , LeaseOwnerState
    , initState
    , propose
    , onPrepareResponse
    , onProposeResponse
    , onTimeout
    ) where

import Data.Maybe (isNothing)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)

import Control.Lens hiding (use, view)
import qualified Control.Lens as Lens

import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.State.Class (MonadState)

import Control.Monad.Free
import Control.Monad.Free.TH

import Control.Monad.Indexed.State (IxMonadState, imodify)

import Control.Monad.Indexed.Prelude

import Network.PaxosLease.Types

leaseTime :: Int
leaseTime = 1000

data ProposerConfig = ProposerConfig { proposerConfigMajority :: !Int
                                     , proposerConfigProposerID :: !()
                                     }
    deriving (Show, Eq, Generic, Typeable)

makeFields ''ProposerConfig

data State = Idle
           | Preparing
           | Proposing
           | LeaseOwner
    deriving (Show, Eq)

newtype IdleState = IdleState { _idleStateBallotNumber :: BallotNumber
                              }
    deriving (Show, Eq)

makeFields ''IdleState

initState :: IdleState
initState = IdleState BallotNumber

data PreparingState = PreparingState { _preparingStateBallotNumber :: !BallotNumber
                                     , _preparingStateNumOpen :: !Int
                                     }
    deriving (Show, Eq)

makeFields ''PreparingState

data ProposingState = ProposingState { _proposingStateBallotNumber :: !BallotNumber
                                     , _proposingStateNumAccepted :: !Int
                                     }
    deriving (Show, Eq)

makeFields ''ProposingState

newtype LeaseOwnerState = LeaseOwnerState { _leaseOwnerStateBallotNumber :: BallotNumber
                                          }
    deriving (Show, Eq)

makeFields ''LeaseOwnerState

type family ProposerState (s :: State) where
    ProposerState 'Idle = IdleState
    ProposerState 'Preparing = PreparingState
    ProposerState 'Proposing = ProposingState
    ProposerState 'LeaseOwner = LeaseOwnerState

data Transition (from :: State) (to :: State) where
    IdleToPreparing :: Transition 'Idle 'Preparing
    PreparingToProposing :: Transition 'Preparing 'Proposing
    ProposingToLeaseOwner :: Transition 'Proposing 'LeaseOwner

deriving instance Show (Transition from to)
deriving instance Eq (Transition from to)

transition :: IxMonadState m
           => Transition from to
           -> m (ProposerState from) (ProposerState to) ()
transition = \case
    IdleToPreparing -> imodify (\s -> PreparingState (s ^. ballotNumber) 0)
    PreparingToProposing -> imodify (\s -> ProposingState (s ^. ballotNumber) 0)
    ProposingToLeaseOwner -> imodify (\s -> LeaseOwnerState (s ^. ballotNumber))


data ProposerF a = BroadcastPrepareRequest PrepareRequest a
                 | BroadcastProposeRequest ProposeRequest a
                 | SetTimeout Int a
    deriving (Functor)

makeFree_ ''ProposerF

broadcastPrepareRequest :: (IxMonad m, MonadFree ProposerF (m i i)) => PrepareRequest -> m i i ()
broadcastProposeRequest :: (IxMonad m, MonadFree ProposerF (m i i)) => ProposeRequest -> m i i ()
setTimeout :: (IxMonad m, MonadFree ProposerF (m i i)) => Int -> m i i ()

nextBallotNumber :: BallotNumber -> BallotNumber
nextBallotNumber BallotNumber = BallotNumber

propose :: ( IxMonadState m
           , MonadState IdleState (m IdleState IdleState)
           , MonadState PreparingState (m PreparingState PreparingState)
           , MonadFree ProposerF (m PreparingState PreparingState)
           )
        => m (ProposerState 'Idle) (ProposerState 'Preparing) ()
propose = do
    bn <- ballotNumber <%= nextBallotNumber
    transition IdleToPreparing
    let request = PrepareRequest bn
    broadcastPrepareRequest request

use :: MonadState s (m i i) => Getting a s a -> m i i a
use = Lens.use
{-# INLINE use #-}

view :: MonadReader s (m i i) => Getting a s a -> m i i a
view = Lens.view
{-# INLINE view #-}

onPrepareResponse :: ( IxMonadState m
                     , MonadReader ProposerConfig (m PreparingState PreparingState)
                     , MonadReader ProposerConfig (m ProposingState ProposingState)
                     , MonadState PreparingState (m PreparingState PreparingState)
                     , MonadState ProposingState (m ProposingState ProposingState)
                     , MonadFree ProposerF (m ProposingState ProposingState)
                     )
                  => PrepareResponse
                  -> m (ProposerState 'Preparing) (Either (ProposerState 'Preparing) (ProposerState 'Proposing)) ()
onPrepareResponse response = do
    bn <- use ballotNumber
    if response ^. ballotNumber /= bn
    then remainPreparing
    else do
        when (isNothing $ response ^. proposal) $
            numOpen += 1
        no <- use numOpen
        m <- view majority
        if no < m
        then remainPreparing
        else do
            transition PreparingToProposing
            setTimeout leaseTime
            pid <- view proposerID
            let request = ProposeRequest bn (Proposal pid leaseTime)
            broadcastProposeRequest request
            wrapProposing
  where
    remainPreparing = imodify Left
    wrapProposing = imodify Right

onProposeResponse :: ( IxMonadState m
                     , MonadReader ProposerConfig (m ProposingState ProposingState)
                     , MonadState ProposingState (m ProposingState ProposingState)
                     )
                  => ProposeResponse
                  -> m (ProposerState 'Proposing) (Either (ProposerState 'Proposing) (ProposerState 'LeaseOwner)) ()
onProposeResponse response = do
    bn <- use ballotNumber
    if response ^. ballotNumber /= bn
    then remainProposing
    else do
        na <- numAccepted <+= 1
        m <- view majority
        if na < m
        then remainProposing
        else do
            transition ProposingToLeaseOwner
            wrapLeaseOwner
  where
    remainProposing = imodify Left
    wrapLeaseOwner = imodify Right

when :: IxPointed f => Bool -> f i i () -> f i i ()
when p s = if p then s else pure ()
{-# INLINE when #-}

onTimeout :: ( IxMonadState m
             , HasBallotNumber from BallotNumber
             )
          => m from IdleState ()
onTimeout = imodify (\s -> IdleState (s ^. ballotNumber))
