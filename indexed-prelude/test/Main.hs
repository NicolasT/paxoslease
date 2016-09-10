{-# LANGUAGE RebindableSyntax #-}

module Main (main) where

import qualified Prelude

import Control.Monad.Indexed.Prelude

switch :: m i j ()
switch = undefined

printInt :: IxMonad m => Int -> m i i ()
printInt _ = return ()

returnInt :: IxPointed m => m i i Int
returnInt = return 2

_testDo :: IxMonad m => m Int Bool String
_testDo = do
    printInt 1 -- Test (>>)
    switch :: m Int String () -- Test indexed'ness
    v <- returnInt -- Test (>>=)
    printInt v
    switch :: m String Bool ()
    Just _ <- return Nothing -- Test fail
    return "abc"

_testOperators :: IxMonad m => m Int String ()
_testOperators =
    ((< 1) <$> pure (0 :: Int)) >>= \True ->
    (switch :: m Int Int ()) >>
    ((\() () -> ()) <$> pure () <*> pure ()) >>
    (switch :: m Int Bool ()) >>
    return (1 :: Int) >>= \1 ->
    (switch :: m Bool String ()) >>= \() ->
    const () <$> returnInt

main :: IO ()
main = Prelude.return ()
