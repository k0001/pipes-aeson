{-# LANGUAGE RankNTypes #-}

-- | This module allows you to encode and decode JSON values flowing downstream
-- through Pipes streams.
--
-- This module builds on top of the @aeson@, @pipes@ and @pipes-parse@
-- libraries, and assumes you know how to use them.

module Pipes.Aeson
  ( -- * Encoding
    encode

    -- * Decoding
    -- $decoding
  , decode
  , decoded
    -- ** Including lengths
  , decodeL
  , decodedL

    -- * Types
  , I.DecodingError(..)
  ) where

import qualified Data.Aeson                       as Ae
import qualified Data.ByteString.Char8            as B
import           Pipes
import qualified Pipes.Aeson.Internal             as I
import qualified Pipes.Aeson.Unsafe               as U
import qualified Pipes.Attoparsec                 as PA
import qualified Pipes.Parse                      as Pipes

--------------------------------------------------------------------------------

type Lens' s a = forall f . Functor f => (a -> f a) -> (s -> f s)

--------------------------------------------------------------------------------

-- | Encode an 'Ae.Array' or 'Ae.Object' as JSON and send it downstream,
-- possibly in more than one 'B.ByteString' chunk.
--
-- /Note:/ The JSON RFC-4627 standard only allows arrays or objects as top-level
-- entities, which is why this function restricts its input to them. If you
-- prefer to ignore the standard and encode any 'Ae.Value', then use 'U.encode'
-- from the "Pipes.Aeson.Unsafe" module.
--
-- /Hint:/ You can easily turn this 'Producer'' into a 'Pipe' that encodes
-- 'Ae.Array' or 'Ae.Object' values as JSON as they flow downstream using:
--
-- @
-- 'for' 'cat' 'encode' :: ('Monad' m) => 'Pipe' ('Either' 'Ae.Object' 'Ae.Array') 'B.ByteString' m r
-- @
encode :: Monad m => Either Ae.Object Ae.Array -> Producer' B.ByteString m ()
encode (Left  x) = U.encode x
encode (Right x) = U.encode x
{-# INLINABLE encode #-}
{-# RULES "p >-> for cat encode" forall p .
    p >-> for cat encode = for p (\a -> encode a)
  #-}

--------------------------------------------------------------------------------
-- $decoding
--
-- Decoding JSON as a Haskell value in involves two different steps:
--
-- * Parsing a raw JSON 'B.ByteString' into an 'Ae.Object' or an 'Ae.Array'.
--
-- * Converting the obtained 'Ae.Object' or 'Ae.Array' to the desired
-- 'Ae.FromJSON' instance.
--
-- Any of those steps can fail, in which case a 'I.DecodingError' will report
-- the precise error and at which the step it appened.


-- | Decodes an 'Ae.Object' or 'Ae.Array' JSON value from the underlying state.
--
-- Returns either the decoded entitiy and the number of decoded bytes,
-- or a 'I.DecodingError' in case of failures.
--
-- /Do not/ use this function if the underlying 'Producer' has leading empty
-- chunks or whitespace, otherwise you may get unexpected parsing errors.
--
-- /Note:/ The JSON RFC-4627 standard only allows arrays or objects as top-level
-- entities, which is why this 'Producer' restricts its output to them. If you
-- prefer to ignore the standard and decode any 'Ae.Value', then use 'U.decode'
-- from the "Pipes.Aeson.Unsafe" module.
decode
  :: (Monad m, Ae.FromJSON a)
  => Pipes.Parser B.ByteString m (Either I.DecodingError a)
decode = do
    x <- decodeL
    return (case x of
       Left   e     -> Left  e
       Right (_, a) -> Right a)
{-# INLINABLE decode #-}

-- | Like 'decode', but also returns the length of input consumed in order to
-- to decode the value.
decodeL
  :: (Monad m, Ae.FromJSON a)
  => Pipes.Parser B.ByteString m (Either I.DecodingError (Int, a))
decodeL = do
    ev <- PA.parseL Ae.json'
    return (case ev of
       Left  e      -> Left (I.AttoparsecError e)
       Right (n, v) -> case Ae.fromJSON v of
          Ae.Error e   -> Left (I.FromJSONError e)
          Ae.Success a -> Right (n, a))
{-# INLINABLE decodeL #-}

decoded
  :: (Monad m, Ae.FromJSON a)
  => (a -> Either Ae.Object Ae.Array)
  => Lens' (Producer B.ByteString m r)
           (Producer a m (Either (I.DecodingError, Producer B.ByteString m r) r))
decoded f k p = fmap _encode (k (I.consecutively decode p))
  where
    _encode = \p0 -> do
      er <- for p0 (\a -> encode (f a))
      case er of
         Left (_, p1) -> p1
         Right r      -> return r
    {-# INLINE _encode #-}
{-# INLINABLE decoded #-}

decodedL
  :: (Monad m, Ae.FromJSON a)
  => (a -> Either Ae.Object Ae.Array)
  => Lens' (Producer B.ByteString m r)
           (Producer (Int, a) m (Either (I.DecodingError, Producer B.ByteString m r) r))
decodedL f k p = fmap _encode (k (I.consecutively decodeL p))
  where
    _encode = \p0 -> do
      er <- for p0 (\(_, a) -> encode (f a))
      case er of
         Left (_, p1) -> p1
         Right r      -> return r
    {-# INLINE _encode #-}
{-# INLINABLE decodedL #-}

