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
    encodeArray
    , encodeObject

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

import qualified Data.Aeson            as Ae
import qualified Data.ByteString.Char8 as B
import           Pipes
import qualified Pipes.Aeson.Internal  as I
import qualified Pipes.Aeson.Unchecked as U
import qualified Pipes.Attoparsec      as PA
import qualified Pipes.Parse           as Pipes

--------------------------------------------------------------------------------
-- | Encode an 'Ae.Object' as JSON and send it downstream,
encodeObject :: Monad m => Ae.Object -> Producer' B.ByteString m ()
encodeObject = U.encode

-- | Encode an 'Ae.Array' as JSON and send it downstream,
encodeArray :: Monad m => Ae.Array -> Producer' B.ByteString m ()
encodeArray = U.encode

-- | Encode an 'Ae.Array' or 'Ae.Object' as JSON and send it downstream,
-- possibly in more than one 'B.ByteString' chunk.
--
-- /Note:/ The JSON RFC-4627 standard only allows arrays or objects as top-level
-- entities, which is why this function restricts its input to them. If you
-- prefer to ignore the standard and encode any 'Ae.Value', then use 'U.encode'
-- from the "Pipes.Aeson.Unchecked" module.
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
-- Decoding JSON as a Haskell value involves two different steps:
--
-- * Parsing a raw JSON 'B.ByteString' into an 'Ae.Object' or an 'Ae.Array'.
--
-- * Converting the obtained 'Ae.Object' or 'Ae.Array' to the desired
-- 'Ae.FromJSON' instance.
--
-- Any of those steps can fail, in which case a 'I.DecodingError' will report
-- the precise error and at which the step it happened.


-- | Decodes an 'Ae.Object' or 'Ae.Array' JSON value from the underlying state.
--
-- Returns either the decoded entitiy, or a 'I.DecodingError' in case of error.
--
-- /Do not/ use this function if the underlying 'Producer' has leading empty
-- chunks or whitespace, otherwise you may get unexpected parsing errors.
--
-- /Note:/ The JSON RFC-4627 standard only allows arrays or objects as top-level
-- entities, which is why this 'Pipes.Parser' restricts its output to them. If
-- you prefer to ignore the standard and decode any 'Ae.Value', then use
-- 'U.decode' from the "Pipes.Aeson.Unchecked" module.
decode
  :: (Monad m, Ae.FromJSON a)
  => Pipes.Parser B.ByteString m (Either I.DecodingError a)
decode = do
    x <- decodeL
    return (case x of
       Left   e     -> Left  e
       Right (_, a) -> Right a)
{-# INLINABLE decode #-}

-- | Like 'decode', except it also returns the length of JSON input that was
-- consumed in order to obtain the value, not including the length of whitespace
-- before nor after the parsed JSON input.
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
       er <- for p (\a -> encode (f (Ae.toJSON a)))
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
      er <- for p (\(_, a) -> encode (f (Ae.toJSON a)))
      case er of
         Left (_, p') -> p'
         Right r      -> return r
{-# INLINABLE decodedL #-}


--------------------------------------------------------------------------------
-- Internal tools --------------------------------------------------------------

type Lens' s a = forall f . Functor f => (a -> f a) -> (s -> f s)

