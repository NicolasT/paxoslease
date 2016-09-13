{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Control.Monad.Indexed.State.Orphans (
    ) where

import Control.Monad.Free (MonadFree(..))

import Control.Monad.Indexed.State (IxStateT(..))

instance (Functor f, MonadFree f m) => MonadFree f (IxStateT m i i) where
    wrap fm = IxStateT $ \s -> wrap $ flip runIxStateT s <$> fm
    {-# INLINE wrap #-}
