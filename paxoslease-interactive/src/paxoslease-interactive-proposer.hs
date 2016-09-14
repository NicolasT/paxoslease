{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Control.Applicative
import Text.Read (readMaybe)

import Control.Monad.Trans.Reader (runReaderT)

import Control.Monad.Free (iterM)

import Network.PaxosLease.Proposer

import Lib (step)

parseEvent :: String -> Maybe Event
parseEvent input =  ReceivedPrepareResponse <$> readMaybe input
                <|> ReceivedProposeResponse <$> readMaybe input
                <|> if input == "Timeout" then Just Timeout else Nothing

handleEffect :: ProposerF (IO a) -> IO a
handleEffect = \case
    BroadcastPrepareRequest request a -> do
        putStr "Broadcast: "
        print request
        a
    BroadcastProposeRequest request a -> do
        putStr "Broadcast: "
        print request
        a
    SetTimeout i a -> do
        putStrLn $ "SetTimeout: " ++ show i
        a

main :: IO ()
main = do
    state <- iterM handleEffect $ flip runReaderT config $ propose initState
    step parseEvent handleEvent (iterM handleEffect . flip runReaderT config) state
  where
    config = ProposerConfig { proposerConfigMajority = 2
                            , proposerConfigProposerID = ()
                            }
