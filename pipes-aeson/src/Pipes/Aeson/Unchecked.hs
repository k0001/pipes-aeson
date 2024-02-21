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
  , loop
    -- ** Including lenghts
  , decodeL
  , decodedL
  , loopL
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
-- encode :: (Monad m, Ae.ToJSON a) => a -> Producer' B.ByteString m ()
encode :: (Monad m, Ae.ToJSON a) => a -> Proxy x' x () PB.ByteString m ()
encode a = PB.fromLazy (Ae.encode a)
{-# INLINABLE encode #-}
{-# RULES "p >-> for cat encode" forall p .
    p >-> for cat encode = for p (\a -> PB.fromLazy (Ae.encode a))
  #-}

--------------------------------------------------------------------------------

-- | Like 'Pipes.Aeson.decode', except it will decode any 'Ae.FromJSON'
-- instance, not just 'Ae.Array' or 'Ae.Object'.
decode
  :: (Monad m, Ae.FromJSON a)
  => Pipes.Parser B.ByteString m (Maybe (Either I.DecodingError a)) -- ^
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


-- | Repeteadly try to parse raw JSON bytes into @a@ values, reporting any
-- 'I.DecodingError's downstream as they happen.
loop
  :: (Monad m, Ae.FromJSON a)
  => (Pipes.Producer B.ByteString m r -> Pipes.Producer B.ByteString m r)
  -- ^ In case of 'I.AttoparsecError', this function will be called to modify
  -- the leftovers 'Pipes.Producer' before using it.
  --
  -- Ideally you will want to drop everything until the beginning of the next
  -- JSON element. This is easy to accomplish if there is a clear whitespace
  -- delimiter between the JSON elements, such as a newline (i.e.,
  -- @'Pipes.ByteString.drop' 1 . 'Pipes.ByteString.dropWhile' (/= 0xA)@).
  -- However, it can be hard to do correctly is there is no such delimiter.
  -- Skipping the first character (i.e., @'Pipes.ByteString.drop' 1@) should be
  -- sufficient in most cases, but not when parsing recursive data structures
  -- because you can accidentally parse a child in its parent's stead.
  --
  -- Notice that unless you advance the 'Pipes.Producer' somehow, 'loop'
  -- will never terminate.
  -> Pipes.Producer B.ByteString m r
  -- ^ Raw JSON input.
  -> Pipes.Producer' (Either I.DecodingError a) m r
{-# INLINABLE loop #-}
loop fp p0 = Pipes.for (loopL fp p0) (Pipes.yield . fmap snd)

-- | Like 'loop', except it also outputs the length of JSON input that was
-- consumed in order to obtain the value, not including the length of whitespace
-- before nor after the parsed JSON input.
loopL
  :: (Monad m, Ae.FromJSON a)
  => (Pipes.Producer B.ByteString m r -> Pipes.Producer B.ByteString m r)
  -- ^ In case of 'I.AttoparsecError', this function will be called to modify
  -- the leftovers 'Pipes.Producer' before using it.
  --
  -- Ideally you will want to drop everything until the beginning of the next
  -- JSON element. This is easy to accomplish if there is a clear whitespace
  -- delimiter between the JSON elements, such as a newline (i.e.,
  -- @'Pipes.ByteString.drop' 1 . 'Pipes.ByteString.dropWhile' (/= 0xA)@).
  -- However, it can be hard to do correctly is there is no such delimiter.
  -- Skipping the first character (i.e., @'Pipes.ByteString.drop' 1@) should be
  -- sufficient in most cases, but not when parsing recursive data structures
  -- because you can accidentally parse a child in its parent's stead.
  --
  -- Notice that unless you advance the 'Pipes.Producer' somehow, 'loopL'
  -- will never terminate.
  -> Pipes.Producer B.ByteString m r
  -- ^ Raw JSON input.
  -> Pipes.Proxy x' x () (Either I.DecodingError (Int, a)) m r
{-# INLINABLE loopL #-}
loopL = I.loopL Ae.value'

--------------------------------------------------------------------------------
-- Internal tools --------------------------------------------------------------

type Lens' s a = forall f . Functor f => (a -> f a) -> s -> f s
