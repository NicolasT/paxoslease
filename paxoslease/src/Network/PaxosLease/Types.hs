{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.PaxosLease.Types where

import Data.Typeable (Typeable)
import GHC.Generics (Generic)

import Control.Lens

data BallotNumber = BallotNumber
    deriving (Show, Read, Eq, Ord, Generic, Typeable)

ballotNumber0 :: BallotNumber
ballotNumber0 = BallotNumber

data Proposal = Proposal { proposalProposerID :: ()
                         , proposalTimeout :: Int
                         }
    deriving (Show, Read, Eq, Generic, Typeable)

makeFields ''Proposal

data PrepareRequest = PrepareRequest { prepareRequestBallotNumber :: BallotNumber
                                     }
    deriving (Show, Read, Eq, Generic, Typeable)

makeFields ''PrepareRequest

data PrepareResponse = PrepareResponse { prepareResponseBallotNumber :: BallotNumber
                                       , prepareResponseProposal :: Maybe Proposal
                                       }
    deriving (Show, Read, Eq, Generic, Typeable)

makeFields ''PrepareResponse

data ProposeRequest = ProposeRequest { proposeRequestBallotNumber :: BallotNumber
                                     , proposeRequestProposal :: Proposal
                                     }
    deriving (Show, Read, Eq, Generic, Typeable)

makeFields ''ProposeRequest

data ProposeResponse = ProposeResponse { proposeResponseBallotNumber :: BallotNumber
                                       }
    deriving (Show, Read, Eq, Generic, Typeable)

makeFields ''ProposeResponse
