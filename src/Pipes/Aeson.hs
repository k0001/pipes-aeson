{-# LANGUAGE RankNTypes #-}

-- | This module allows you to encode and decode JSON values flowing downstream
-- through Pipes streams.
--
-- This module builds on top of the @aeson@, @pipes@ and @pipes-parse@
-- libraries, and assumes you know how to use them. Please read the examples
-- in "Pipes.Parse.Tutorial" to understand how to use these functions.
--
-- In this module, the following type synonym compatible with the @lens@,
-- @lens-family@ and @lens-family-core@ libraries is used but not exported:
--
-- @
-- type Lens' s a = forall f . 'Functor' f => (a -> f a) -> (s -> f s)
-- @

module Pipes.Aeson
  ( -- * Encoding
    -- $encoding
    encodeArray
  , encodeObject

    -- * Decoding
    -- $decoding
  , decode
  , decoded
  , loop
    -- ** Including lengths
  , decodeL
  , decodedL
  , loopL

    -- * Types
  , I.DecodingError(..)
  ) where

import qualified Data.Aeson            as Ae
import qualified Data.ByteString.Char8 as B
import           Pipes
import qualified Pipes.Aeson.Internal  as I
import qualified Pipes.Aeson.Unchecked as U
import qualified Pipes.Parse           as Pipes

--------------------------------------------------------------------------------
-- $encoding
--
-- Encode 'Ae.Array' or 'Ae.Object' values as JSON and send them downstream,
-- possibly in more than one 'B.ByteString' chunk.
--
-- /Note:/ The JSON RFC-4627 standard only allows arrays or objects as top-level
-- entities, which is why these functions restrict their input to them. If you
-- prefer to ignore the standard and encode any 'Ae.Value', then use 'U.encode'
-- from the "Pipes.Aeson.Unchecked" module.
--

-- | Encode an 'Ae.Object' as JSON and send it downstream,
--
-- /Hint:/ You can easily turn this 'Producer'' into a 'Pipe' that encodes
-- 'Ae.Object' values as JSON as they flow downstream using:
--
-- @
-- 'for' 'cat' 'encodeObject' :: 'Monad' m => 'Pipe' 'Ae.Object' 'B.ByteString' m r
-- @
encodeObject :: Monad m => Ae.Object -> Proxy x' x () B.ByteString m ()
encodeObject = U.encode
{-# INLINABLE encodeObject #-}
{-# RULES "p >-> for cat encodeObject" forall p .
    p >-> for cat encodeObject = for p encodeObject
  #-}

-- | Encode an 'Ae.Array' as JSON and send it downstream,
--
-- /Hint:/ You can easily turn this 'Producer'' into a 'Pipe' that encodes
-- 'Ae.Array' values as JSON as they flow downstream using:
--
-- @
-- 'for' 'cat' 'encodeArray' :: 'Monad' m => 'Pipe' 'Ae.Array' 'B.ByteString' m r
-- @
encodeArray :: Monad m => Ae.Array -> Proxy x' x () B.ByteString m ()
encodeArray = U.encode
{-# INLINABLE encodeArray #-}
{-# RULES "p >-> for cat encodeArray" forall p .
    p >-> for cat encodeArray = for p encodeArray
  #-}

--------------------------------------------------------------------------------
-- $decoding
--
-- Decoding JSON as a Haskell value involves two different steps:
--
-- * Parsing a raw JSON 'B.ByteString' into an 'Ae.Object' or an 'Ae.Array'.
--
-- * Converting the obtained 'Ae.Object' or 'Ae.Array' to the desired
-- 'Ae.FromJSON' instance.
--
-- Any of those steps can fail, in which case a 'I.DecodingError' will report
-- the precise error and at which step it happened.


-- | Decodes an 'Ae.Object' or 'Ae.Array' JSON value from the underlying state.
--
-- It returns 'Nothing' if the underlying 'Producer' is exhausted, otherwise
-- it returns either the decoded entity or a 'I.DecodingError' in case of error.
--
-- /Note:/ The JSON RFC-4627 standard only allows arrays or objects as top-level
-- entities, which is why this 'Pipes.Parser' restricts its output to them. If
-- you prefer to ignore the standard and decode any 'Ae.Value', then use
-- 'U.decode' from the "Pipes.Aeson.Unchecked" module.
decode
  :: (Monad m, Ae.FromJSON a)
  => Pipes.Parser B.ByteString m (Maybe (Either I.DecodingError a))
decode = fmap (fmap (fmap snd)) decodeL
{-# INLINABLE decode #-}

-- | Like 'decode', except it also returns the length of JSON input that was
-- consumed in order to obtain the value, not including the length of whitespace
-- before nor after the parsed JSON input.
decodeL
  :: (Monad m, Ae.FromJSON a)
  => Pipes.Parser B.ByteString m (Maybe (Either I.DecodingError (Int, a)))
decodeL = I.decodeL Ae.json'
{-# INLINABLE decodeL #-}


-- | /Improper lens/ that turns a stream of raw JSON input into a stream of
-- 'Ae.FromJSON' and back.
--
-- By /improper lens/ we mean that in practice you can't expect the
-- /Monad Morphism Laws/ to be true when using 'decoded' with
-- 'Control.Lens.zoom'.
--
-- @
-- 'Control.Lens.zoom' 'decoded' ('return' r) /= 'return' r
-- 'Control.Lens.zoom' 'decoded' (m >>= k)  /= 'Control.Lens.zoom' m >>= 'Control.Lens.zoom' . f
-- @
--
-- /Note:/ The JSON RFC-4627 standard only allows arrays or objects as top-level
-- entities, which is why this function restricts its stream values to them. If
-- you prefer to ignore the standard and encode or decode any 'Ae.Value', then
-- use 'U.decoded' from the "Pipes.Aeson.Unchecked" module.
decoded
  :: (Monad m, Ae.FromJSON a, Ae.ToJSON a)
  => (Ae.Value -> Either Ae.Object Ae.Array)
     -- ^ A witness that @a@ can be represented either as an 'Ae.Object' or as
     -- an 'Ae.Array'. The passed in 'Ae.Value' is @'Ae.toJSON' a@
  -> Lens' (Producer B.ByteString m r)
           (Producer a m (Either (I.DecodingError, Producer B.ByteString m r) r))
decoded f k p0 = fmap _encode (k (I.consecutively decode p0))
  where
    _encode = \p -> do
       er <- for p (\a -> either encodeObject encodeArray (f (Ae.toJSON a)))
       case er of
          Left (_, p') -> p'
          Right r      -> return r
{-# INLINABLE decoded #-}

-- | Like 'decoded', except it also tags each decoded entity with the length of
-- JSON input that was consumed in order to obtain the value, not including the
-- length of whitespace between each parsed JSON input.
decodedL
  :: (Monad m, Ae.FromJSON a, Ae.ToJSON a)
  => (Ae.Value -> Either Ae.Object Ae.Array)
     -- ^ A witness that @a@ can be represented either as an 'Ae.Object' or as
     -- an 'Ae.Array'. The passed in 'Ae.Value' is @'Ae.toJSON' a@
  -> Lens' (Producer B.ByteString m r)
           (Producer (Int, a) m (Either (I.DecodingError, Producer B.ByteString m r) r))
decodedL f k p0 = fmap _encode (k (I.consecutively decode p0))
  where
    _encode = \p -> do
      er <- for p (\(_, a) -> either encodeObject encodeArray (f (Ae.toJSON a)))
      case er of
         Left (_, p') -> p'
         Right r      -> return r
{-# INLINABLE decodedL #-}

-- | Repeteadly try to parse raw JSON bytes into @a@ values, reporting any
-- 'I.DecodingError's downstream as they happen.
--
-- /Note:/ The JSON RFC-4627 standard only allows arrays or objects as top-level
-- entities, which is why these functions restrict their input to them. If you
-- prefer to ignore the standard and encode any 'Ae.Value', then use 'U.encode'
-- from the "Pipes.Aeson.Unchecked" module.
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
loopL = I.loopL Ae.json'

--------------------------------------------------------------------------------
-- Internal tools --------------------------------------------------------------

type Lens' s a = forall f . Functor f => (a -> f a) -> s -> f s
