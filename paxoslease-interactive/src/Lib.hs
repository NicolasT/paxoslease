module Lib (step) where

import System.IO (hFlush, stdout)

step :: ( Show state
        , Show event
        )
     => (String -> Maybe event)
     -> (state -> event -> action)
     -> (action -> IO state)
     -> state
     -> IO a
step parseEvent handleEvent handleEffects = loop
  where
    loop state = do
        putStrLn "Current State"
        putStrLn "============="
        print state
        putStrLn ""

        putStrLn "Event"
        putStrLn "====="
        putStr "> "; hFlush stdout
        event <- parseEvent `fmap` getLine

        case event of
            Nothing -> do
                putStrLn "Unable to parse input"
                putStrLn ""
                loop state
            Just event' -> do
                putStrLn $ "Processing event: " ++ show event'
                state' <- handleEffects $ handleEvent state event'
                putStrLn ""
                loop state'

