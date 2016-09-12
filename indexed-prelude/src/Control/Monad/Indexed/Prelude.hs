{-|
Module      : Control.Monad.Indexed.Prelude
Description : A minimal @Prelude@ for indexed monads
Copyright   : (c) 2016, Nicolas Trangez
License     : BSD3
Maintainer  : ikke@nicolast.be

Combined with the @RebindableSyntax@ language extension, this module allows to
use standard @do@-notation in the context of /indexed/ monads.
-}

module Control.Monad.Indexed.Prelude (
    -- * Support for @RebindableSyntax@ (with 'fail' = 'error')
    (>>), (>>=), fail, ifThenElse,
    -- * 'Functor', 'Applicative' and 'Monad'-like operators
    (<$>), (<$), (<*>), (<*), (*>), pure, (=<<), return,
    -- * Re-exports
    -- ** Standard @Prelude@, without names defined in this module
    module Prelude,
    -- ** All of @Control.Monad.Indexed@
    module Control.Monad.Indexed,
    ) where

import Prelude hiding (
    (>>), (>>=), fail,
    (<$>), (<$), (<*>), (<*), (*>), pure, (=<<), return,
    )

import Control.Monad.Indexed

-- | Indexed version of 'Prelude.>>='.
(>>=) :: IxMonad m => m i j a -> (a -> m j k b) -> m i k b
(>>=) = (>>>=)
{-# INLINE (>>=) #-}

-- | Indexed version of 'Prelude.>>'.
(>>) :: IxMonad m => m i j a -> m j k b -> m i k b
a >> b = a >>= const b
{-# INLINE (>>) #-}

-- | `fail` is like 'Prelude.error'.
fail :: String -> m i j a
fail s = error $ "fail: " ++ s
{-# INLINE fail #-}

-- | Standard implementation for conditional constructs.
ifThenElse :: Bool -> a -> a -> a
ifThenElse b a1 a2 = if b then a1 else a2
{-# INLINE ifThenElse #-}

-- | Indexed version of 'Prelude.<$>'.
(<$>) :: IxFunctor f => (a -> b) -> f i j a -> f i j b
(<$>) = imap
{-# INLINE (<$>) #-}

-- | Indexed version of 'Prelude.<$'.
(<$) :: IxFunctor f => a -> f i j b -> f i j a
(<$) = imap . const
{-# INLINE (<$) #-}

-- | Indexed version of 'Prelude.<*>'.
(<*>) :: IxApplicative f => f i j (a -> b) -> f j k a -> f i k b
(<*>) = iap
{-# INLINE (<*>) #-}

-- | Indexed version of 'Prelude.<*'.
(<*) :: IxApplicative f => f i j a -> f j k b -> f i k a
a <* b = imap const a `iap` b
{-# INLINE (<*) #-}

-- | Indexed version of 'Prelude.*>'.
(*>) :: IxApplicative f => f i j a -> f j k b -> f i k b
a *> b = (const id `imap` a) `iap` b
{-# INLINE (*>) #-}

-- | Indexed version of 'Prelude.pure'.
pure :: IxPointed f => a -> f i i a
pure = ireturn
{-# INLINE pure #-}

-- | Indexed version of 'Prelude.=<<'.
(=<<) :: IxMonad m => (a -> m j k b) -> m i j a -> m i k b
(=<<) = flip (>>>=)
{-# INLINE (=<<) #-}

-- | Indexed version of 'Prelude.return'.
return :: IxPointed m => a -> m i i a
return = ireturn
{-# INLINE return #-}
