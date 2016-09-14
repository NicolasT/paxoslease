{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Control.Applicative
import Text.Read (readMaybe)

import Control.Monad.Free (iterM)

import Network.PaxosLease.Acceptor

import Lib (step)

parseEvent :: String -> Maybe Event
parseEvent input =  ReceivedPrepareRequest <$> readMaybe input
                <|> ReceivedProposeRequest <$> readMaybe input
                <|> if input == "Timeout" then Just Timeout else Nothing

handleEffect :: AcceptorF (IO a) -> IO a
handleEffect = \case
    SendPrepareResponse response a -> do
        putStr "Send: "
        print response
        a
    SendProposeResponse response a -> do
        putStr "Send: "
        print response
        a
    SetTimeout i a -> do
        putStrLn $ "SetTimeout: " ++ show i
        a

main :: IO ()
main = step parseEvent handleEvent (iterM handleEffect) initState
