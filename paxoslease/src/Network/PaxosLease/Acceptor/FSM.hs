{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.PaxosLease.Acceptor.FSM where

import Control.Monad (unless)
import Data.Typeable (Typeable)

import Control.Monad.State.Class (MonadState)

import Control.Lens

import Control.Monad.Free
import Control.Monad.Free.TH

import Network.PaxosLease.Types

data AcceptorState = AcceptorState { _highestPromised :: BallotNumber
                                   , _acceptedProposal :: Maybe Proposal
                                   }
    deriving (Show, Eq, Typeable)

makeLenses ''AcceptorState

data AcceptorF a = SendPrepareResponse PrepareResponse  a
                 | SendProposeResponse ProposeResponse a
                 | SetTimeout Int a
    deriving (Functor)

makeFree ''AcceptorF

initState :: AcceptorState
initState = AcceptorState { _highestPromised = ballotNumber0
                          , _acceptedProposal = Nothing
                          }

onPrepareRequest :: (MonadState AcceptorState m, MonadFree AcceptorF m)
                 => PrepareRequest
                 -> m ()
onPrepareRequest request = do
    hp <- use highestPromised
    unless (request ^. ballotNumber < hp) $ do
        highestPromised .= request ^. ballotNumber
        response <- PrepareResponse     (request ^. ballotNumber)
                                    <$> use acceptedProposal
        sendPrepareResponse response

onProposeRequest :: (MonadState AcceptorState m, MonadFree AcceptorF m)
                 => ProposeRequest
                 -> m ()
onProposeRequest request = do
    hp <- use highestPromised
    unless (request ^. ballotNumber < hp) $ do
        acceptedProposal .= Just (request ^. proposal)
        setTimeout (request ^. proposal . timeout)
        let response = ProposeResponse (request ^. ballotNumber)
        sendProposeResponse response

onTimeout :: MonadState AcceptorState m
          => m ()
onTimeout =
    acceptedProposal .= Nothing
