{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Network.PaxosLease.Acceptor (
      AcceptorState
    , initState
    , Event(..)
    , handleEvent
    , AcceptorF(..)

    , PrepareRequest(..)
    , PrepareResponse(..)
    , ProposeRequest(..)
    , ProposeResponse(..)

    , BallotNumber(..)
    , Proposal(..)
    ) where

import Data.Typeable (Typeable)

import Control.Monad.Trans.State (execStateT)

import Control.Monad.Free (MonadFree)

import Network.PaxosLease.Acceptor.FSM
import Network.PaxosLease.Types

data Event = ReceivedPrepareRequest PrepareRequest
           | ReceivedProposeRequest ProposeRequest
           | Timeout
    deriving (Show, Eq, Typeable)

handleEvent :: ( Monad m
               , MonadFree AcceptorF m
               )
            => AcceptorState
            -> Event
            -> m AcceptorState
handleEvent state = \case
    ReceivedPrepareRequest request -> execStateT (onPrepareRequest request) state
    ReceivedProposeRequest request -> execStateT (onProposeRequest request) state
    Timeout -> execStateT onTimeout state
