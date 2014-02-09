{-# LANGUAGE RankNTypes #-}

-- | This module exports facilities similar to those exported by the
-- "Pipes.Aeson" module, except they do not restrict the 'Ae.Value's
-- that might be encoded or decoded to be just valid top-level values. That is,
-- not only 'Ae.Object's or 'Ae.Array's, according to to the RFC-4627 JSON
-- standard.

module Pipes.Aeson.Unsafe
  ( -- * Encoding
    encode
    -- * Decoding
  , decode
  , decoded
    -- ** Including lenghts
  , decodeL
  , decodedL
  ) where

import           Pipes
import qualified Pipes.Parse as Pipes
import qualified Pipes.Aeson.Internal             as I
import qualified Pipes.Attoparsec                 as PA
import qualified Data.Aeson                       as Ae
import qualified Data.Aeson.Parser                as Ae (value')
import qualified Data.ByteString                  as B

--------------------------------------------------------------------------------

type Lens' s a = forall f . Functor f => (a -> f a) -> (s -> f s)

--------------------------------------------------------------------------------

-- | Like 'Pipes.Aeson.encode', except it accepts any 'Ae.ToJSON' instance,
-- not just 'Ae.Array' or 'Ae.Object'.
encode :: (Monad m, Ae.ToJSON a) => a -> Producer' B.ByteString m ()
encode = I.fromLazy . Ae.encode
{-# INLINABLE encode #-}
{-# RULES "p >-> for cat encode" forall p .
    p >-> for cat encode = for p (\a -> I.fromLazy (Ae.encode a))
  #-}

--------------------------------------------------------------------------------

-- | Like 'Pipes.Aeson.decode', except it will decode any 'Ae.ToJSON' instance,
-- not just 'Ae.Array' or 'Ae.Object'.
decode
  :: (Monad m, Ae.FromJSON a)
  => Pipes.Parser B.ByteString m (Either I.DecodingError a)
decode = do
    x <- decodeL
    return (case x of
       Left   e     -> Left e
       Right (_, a) -> Right a)
{-# INLINABLE decode #-}


-- | Like 'decode', but also returns the length of input consumed to parse the
-- value.
decodeL
  :: (Monad m, Ae.FromJSON a)
  => Pipes.Parser B.ByteString m (Either I.DecodingError (Int, a))
decodeL = do
    ev <- PA.parseL Ae.value'
    return (case ev of
       Left  e      -> Left (I.AttoparsecError e)
       Right (n, v) -> case Ae.fromJSON v of
          Ae.Error e   -> Left (I.FromJSONError e)
          Ae.Success a -> Right (n, a))
{-# INLINABLE decodeL #-}

decoded
  :: (Monad m, Ae.FromJSON a, Ae.ToJSON a)
  => Lens' (Producer B.ByteString m r)
           (Producer a m (Either (I.DecodingError, Producer B.ByteString m r) r))
decoded k p = fmap _encode (k (I.consecutively decode p))
  where
    _encode = \p0 -> do
      er <- for p0 (\a -> encode a)
      case er of
         Left (_, p1) -> p1
         Right r      -> return r
    {-# INLINE _encode #-}
{-# INLINABLE decoded #-}


decodedL
  :: (Monad m, Ae.FromJSON a, Ae.ToJSON a)
  => Lens' (Producer B.ByteString m r)
           (Producer (Int, a) m (Either (I.DecodingError, Producer B.ByteString m r) r))
decodedL k p = fmap _encode (k (I.consecutively decodeL p))
  where
    _encode = \p0 -> do
      er <- for p0 (\(_, a) -> encode a)
      case er of
         Left (_, p1) -> p1
         Right r      -> return r
    {-# INLINE _encode #-}
{-# INLINABLE decodedL #-}

