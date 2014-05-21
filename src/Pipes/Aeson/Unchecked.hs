{-# LANGUAGE RankNTypes #-}

-- | This module exports facilities similar to those exported by the
-- "Pipes.Aeson" module, except they do not restrict the 'Ae.Value's
-- that might be encoded or decoded to be just valid top-level values. That is,
-- not only 'Ae.Object's or 'Ae.Array's, according to to the RFC-4627 JSON
-- standard.

module Pipes.Aeson.Unchecked
  ( -- * Encoding
    encode
    -- * Decoding
  , decode
  , decoded
    -- ** Including lenghts
  , decodeL
  , decodedL
  ) where

import           Control.Monad        (liftM)
import qualified Data.Aeson           as Ae
import qualified Data.Aeson.Parser    as Ae (value')
import qualified Data.ByteString      as B
import           Pipes
import qualified Pipes.Aeson.Internal as I
import qualified Pipes.ByteString     as PB
import qualified Pipes.Parse          as Pipes

--------------------------------------------------------------------------------

-- | Like 'Pipes.Aeson.encode', except it accepts any 'Ae.ToJSON' instance,
-- not just 'Ae.Array' or 'Ae.Object'.
encode :: (Monad m, Ae.ToJSON a) => a -> Producer' B.ByteString m ()
encode = PB.fromLazy . Ae.encode
{-# INLINABLE encode #-}
{-# RULES "p >-> for cat encode" forall p .
    p >-> for cat encode = for p (\a -> PB.fromLazy (Ae.encode a))
  #-}

--------------------------------------------------------------------------------

-- | Like 'Pipes.Aeson.decode', except it will decode any 'Ae.FromJSON'
-- instance, not just 'Ae.Array' or 'Ae.Object'.
decode
  :: (Monad m, Ae.FromJSON a)
  => Pipes.Parser B.ByteString m ((Maybe (Either I.DecodingError a))) -- ^
decode = fmap (fmap snd) `liftM` decodeL
{-# INLINABLE decode #-}


-- | Like 'decode', except it also returns the length of JSON input that was
-- consumed in order to obtain the value, not including the length of whitespace
-- between each parsed JSON input.
decodeL
  :: (Monad m, Ae.FromJSON a)
  => Pipes.Parser B.ByteString m (Maybe (Either I.DecodingError (Int, a))) -- ^
decodeL = I.decodeL Ae.value'
{-# INLINABLE decodeL #-}

-- | Like 'Pipes.Aeson.decoded', except it will decode and decode any
-- 'Ae.FromJSON' and 'Ae.ToJSON' instance, not just 'Ae.Array' or 'Ae.Object'.
decoded
  :: (Monad m, Ae.FromJSON a, Ae.ToJSON a)
  => Lens' (Producer B.ByteString m r)
           (Producer a m (Either (I.DecodingError, Producer B.ByteString m r) r))
     -- ^
decoded k p = fmap _encode (k (I.consecutively decode p))
  where
    _encode = \p0 -> do
      er <- for p0 (\a -> encode a)
      case er of
         Left (_, p1) -> p1
         Right r      -> return r
    {-# INLINE _encode #-}
{-# INLINABLE decoded #-}


-- | Like 'decoded', except it also tags each decoded entity with the length of
-- JSON input that was consumed in order to obtain the value, not including the
-- length of whitespace between each parsed JSON input.
decodedL
  :: (Monad m, Ae.FromJSON a, Ae.ToJSON a)
  => Lens' (Producer B.ByteString m r)
           (Producer (Int, a) m (Either (I.DecodingError, Producer B.ByteString m r) r))
     -- ^
decodedL k p = fmap _encode (k (I.consecutively decodeL p))
  where
    _encode = \p0 -> do
      er <- for p0 (\(_, a) -> encode a)
      case er of
         Left (_, p1) -> p1
         Right r      -> return r
    {-# INLINE _encode #-}
{-# INLINABLE decodedL #-}


--------------------------------------------------------------------------------
-- Internal tools --------------------------------------------------------------

type Lens' s a = forall f . Functor f => (a -> f a) -> (s -> f s)
